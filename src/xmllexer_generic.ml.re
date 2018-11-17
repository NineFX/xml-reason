/*
 * (c) 2007-2014 Anastasia Gornostaeva
 */

module type MONAD = {
  type t('a);
  let return: 'a => t('a);
  let (>>=): (t('a), 'a => t('b)) => t('b);
  let fail: exn => t('a);
};

module type ENCODING = {let encode_unicode: int => list(char);};

module type STREAM = {
  include MONAD;
  type stream;
  let set_decoder: (string, stream) => unit;
  let next_char: (stream, unit => t('b), int => t('b)) => t('b);
  let error: (~stream: stream=?, exn) => t('a);
};

type external_id =
  | SystemID(string)
  | PublicID(string, string);

type dtd_attr_type = [ | `CDATA | `ID | `NMTOKEN];

type dtd_attr_default = [ | `Required | `Implied];

type dtd_gedecl_entity = [
  | `ExternalID(external_id, option(string))
  | `EntityValue(string)
];

type dtd_pedecl_entity = [
  | `ExternalID(external_id)
  | `EntityValue(string)
];

type dtd_entity_type =
  | GEDecl(string, dtd_gedecl_entity)
  | PEDecl(string, dtd_pedecl_entity);

type dtd_element_type = [ | `Empty | `Any];

type dtd =
  | DTD_PI(string, string)
  | DTD_PEReference(string)
  | DTD_ATTLIST(string, list((string, dtd_attr_type, dtd_attr_default)))
  | DTD_Entity(dtd_entity_type)
  | DTD_Element(string, dtd_element_type);

type doctype = {
  dtd_name: string,
  dtd_external_id: option(external_id),
  dtd: list(dtd),
};

module type XMLTOKEN = {
  type t('a);
  type token;

  let emit_pi: (string, string) => t(token);
  let emit_start_tag: (string, list((string, string)), bool) => t(token);
  let emit_end_tag: string => t(token);
  let emit_doctype: doctype => t(token);
  let emit_text: string => t(token);
  let emit_eof: unit => t(token);
};

let u_nl = 10;
let u_cr = 13;

let u_space = 32;
let u_excl = 33;
let u_quot = 34;
let u_sharp = 35;
let u_dollar = 36;
let u_percent = 37;
let u_amp = 38;
let u_apos = 39;
let u_lparen = 40;
let u_rparen = 41;
let u_star = 42;
let u_plus = 43;
let u_comma = 44;
let u_dash = 45;
let u_dot = 46;
let u_slash = 47;

let u_1 = 49;
let u_9 = 57;
let u_colon = 58;
let u_semicolon = 59;
let u_lt = 60;
let u_eq = 61;
let u_gt = 62;
let u_qmark = 63;

let u_lbracket = 91;
let u_rbracket = 93;
let u_underline = 95;

let u_x = 120;
let u_bom = 65279;

let u_at = 64;
let u_A = 65;
let u_B = 66;
let u_C = 67;
let u_D = 68;
let u_E = 69;
let u_F = 70;
let u_I = 73;
let u_K = 75;
let u_L = 76;
let u_M = 77;
let u_N = 78;
let u_O = 79;
let u_P = 80;
let u_Q = 81;
let u_R = 82;
let u_S = 83;
let u_T = 84;
let u_U = 85;
let u_Y = 89;

let u_a = 97;
let u_e = 101;
let u_g = 103;
let u_i = 105;
let u_l = 108;
let u_m = 109;
let u_n = 110;
let u_o = 111;
let u_p = 112;
let u_q = 113;
let u_r = 114;
let u_s = 115;
let u_t = 116;
let u_u = 117;
let u_v = 118;

let char_range = (c, r1, r2) => c >= r1 && c <= r2;
let one_of = (u, chars) => List.mem(u, chars);

let is_space = u => u == 32 || u == 10 || u == 9 || u == 13;

module type XNAME = {
  let is_name_start_char: int => bool;
  let is_name_char: int => bool;
};

module XName = {
  let is_name_start_char = u =>
    char_range(u, 97, 122)
    || u == u_colon
    || u == u_underline
    || char_range(u, 65, 90)
    || char_range(u, 192, 214)
    || char_range(u, 216, 246)
    || char_range(u, 248, 767)
    || char_range(u, 880, 893)
    || char_range(u, 895, 8191)
    || char_range(u, 8204, 8205)
    || char_range(u, 8304, 8591)
    || char_range(u, 11264, 12271)
    || char_range(u, 12289, 55295)
    || char_range(u, 63744, 64975)
    || char_range(u, 65008, 65533)
    || char_range(u, 65536, 983039);

  let is_name_char = u =>
    is_name_start_char(u)
    || u == u_dash
    || u == u_dot
    || char_range(u, 48, 57)
    || char_range(u, 768, 879)
    || char_range(u, 8255, 8256);
};

module Make =
       (S: STREAM, E: ENCODING, X: XMLTOKEN with type t('a) = S.t('a)) => {
  module E = E;
  module S = S;
  module X = X;
  open S;

  type next_state =
    | PrologXmlDeclState
    | PrologMiscState
    | TextState
    | LessThanSignState
    | AfterElement;

  type state = {
    tmp_buffer: Buffer.t,
    stack: Stack.t(string),
    mutable next_state,
  };

  exception Exn_msg(string);
  exception Exn_EOF;
  exception Exn_ExpectedChar(list(char));
  exception Exn_ExpectedSpace;
  exception Exn_CharToken(int);
  exception Error_XMLDecl;

  let not_eof = () => error(Exn_EOF);

  let rec add_chars = state =>
    fun
    | [] => ()
    | [x, ...xs] => {
        Buffer.add_char(state.tmp_buffer, x);
        add_chars(state, xs);
      };

  let extract_buffer = state => {
    let value = Buffer.contents(state.tmp_buffer);
    Buffer.reset(state.tmp_buffer);
    value;
  };

  let rec consume_sequence = strm =>
    fun
    | [] => S.return()
    | [x, ...xs] =>
      next_char(strm, not_eof, u =>
        if (u == x) {
          consume_sequence(strm, xs);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
  and consume_space = strm =>
    next_char(strm, not_eof, u =>
      if (is_space(u)) {
        S.return();
      } else {
        error(~stream=strm, Exn_ExpectedSpace);
      }
    );

  let parse_xmldecl = data => {
    let len = String.length(data);
    let i = ref(0);
    let rec skip_space = () =>
      if (i^ < len
          && (
            data.[i^] == ' '
            || data.[i^] == '\n'
            || data.[i^] == '\t'
            || data.[i^] == '\r'
          )) {
        incr(i);
        skip_space();
      } else {
        ();
      };

    let get_lim = () =>
      if (i^ < len && data.[i^] == '"' || data.[i^] == '\'') {
        let lim = data.[i^];
        incr(i);
        lim;
      } else {
        raise(Error_XMLDecl);
      };

    let rec seq =
      fun
      | [] => ()
      | [x, ...xs] =>
        if (i^ < len && data.[i^] == x) {
          incr(i);
          seq(xs);
        } else {
          raise(Error_XMLDecl);
        };

    let get_digits = () => {
      let s = i^;
      let rec aux_loop = () =>
        if (i^ < len && data.[i^] >= '0' && data.[i^] <= '9') {
          incr(i);
          aux_loop();
        } else if (i^ > s) {
          String.sub(data, s, i^ - s);
        } else {
          raise(Error_XMLDecl);
        };

      seq(['1', '.']);
      aux_loop();
    };

    let get_encoding = () => {
      let s = i^;
      let () =
        if (i^ < len) {
          switch (data.[i^]) {
          | 'a'..'z'
          | 'A'..'Z' => incr(i)
          | _ => raise(Error_XMLDecl)
          };
        } else {
          raise(Error_XMLDecl);
        };

      let rec aux_loop = () =>
        if (i^ < len) {
          switch (data.[i^]) {
          | 'a'..'z'
          | 'A'..'Z'
          | '0'..'9'
          | '_'
          | '-' =>
            incr(i);
            aux_loop();
          | _ =>
            if (s < i^) {
              String.sub(data, s, i^ - s);
            } else {
              raise(Error_XMLDecl);
            }
          };
        } else {
          raise(Error_XMLDecl);
        };

      aux_loop();
    };

    let get_standalone = () =>
      if (i^ < len) {
        if (data.[i^] == 'y') {
          incr(i);
          seq(['e', 's']);
          "yes";
        } else if (data.[i^] == 'n') {
          incr(i);
          seq(['o']);
          "no";
        } else {
          raise(Error_XMLDecl);
        };
      } else {
        raise(Error_XMLDecl);
      };

    skip_space();
    seq(['v', 'e', 'r', 's', 'i', 'o', 'n']);
    skip_space();
    seq(['=']);
    skip_space();
    let lim = get_lim();
    let version = get_digits();
    seq([lim]);
    skip_space();
    if (i^ == len) {
      (version, None, None);
    } else if (i^ < len && data.[i^] == 'e') {
      incr(i);
      seq(['n', 'c', 'o', 'd', 'i', 'n', 'g']);
      skip_space();
      seq(['=']);
      skip_space();
      let lim = get_lim();
      let encoding = get_encoding();
      seq([lim]);
      skip_space();
      if (i^ == len) {
        (version, Some(encoding), None);
      } else if (i^ < len && data.[i^] == 's') {
        seq(['t', 'a', 'n', 'd', 'a', 'l', 'o', 'n', 'e']);
        skip_space();
        let lim = get_lim();
        let standalone = get_standalone();
        seq([lim]);
        skip_space();
        if (i^ == len) {
          (version, Some(encoding), Some(standalone));
        } else {
          raise(Error_XMLDecl);
        };
      } else {
        raise(Error_XMLDecl);
      };
    } else if (i^ < len && data.[i^] == 's') {
      seq(['t', 'a', 'n', 'd', 'a', 'l', 'o', 'n', 'e']);
      skip_space();
      let lim = get_lim();
      let standalone = get_standalone();
      seq([lim]);
      skip_space();
      if (i^ == len) {
        (version, None, Some(standalone));
      } else {
        raise(Error_XMLDecl);
      };
    } else {
      raise(Error_XMLDecl);
    };
  };

  type d =
    | N(list((int, d)))
    | L(int);

  let entities =
    N([
      (
        u_a,
        N([
          (u_m, N([(u_p, N([(u_semicolon, L(u_amp))]))])),
          (u_p, N([(u_o, N([(u_s, N([(u_semicolon, L(u_apos))]))]))])),
        ]),
      ),
      (u_g, N([(u_t, N([(u_semicolon, L(u_gt))]))])),
      (u_l, N([(u_t, N([(u_semicolon, L(u_lt))]))])),
      (
        u_q,
        N([
          (u_u, N([(u_o, N([(u_t, N([(u_semicolon, L(u_quot))]))]))])),
        ]),
      ),
    ]);

  let character_reference = strm => {
    let rec aux_entity = u =>
      fun
      | N([]) => error(~stream=strm, Exn_CharToken(u))
      | N([(c, t), ...rest]) =>
        if (u == c) {
          switch (t) {
          | N(_) => next_char(strm, not_eof, u => aux_entity(u, t))
          | L(_) => aux_entity(u, t)
          };
        } else {
          aux_entity(u, N(rest));
        }
      | L(c) => S.return(c);

    next_char(strm, not_eof, u =>
      if (u == u_sharp) {
        next_char(strm, not_eof, u =>
          if (u == u_x) {
            let rec get_hex_code = (consumed, acc) =>
              next_char(strm, not_eof, u =>
                if (char_range(u, 48, 57)) {
                  get_hex_code(true, acc * 16 + (u - 48));
                } else if (char_range(u, 65, 70)) {
                  get_hex_code(true, acc * 16 + (u - 55));
                } else if (char_range(u, 97, 102)) {
                  get_hex_code(true, acc * 16 + (u - 87));
                } else if (u == u_semicolon) {
                  if (consumed) {
                    /* test unicode char */
                    S.return(acc);
                  } else {
                    error(~stream=strm, Exn_msg("Expected hex"));
                  };
                } else {
                  error(~stream=strm, Exn_CharToken(u));
                }
              );

            get_hex_code(false, 0);
          } else if (char_range(u, 48, 57)) {
            let rec get_code = (consumed, acc) =>
              next_char(strm, not_eof, u =>
                if (char_range(u, 48, 57)) {
                  get_code(true, acc * 10 + (u - 48));
                } else if (u == u_semicolon) {
                  if (consumed) {
                    /* test unicode char */
                    S.return(acc);
                  } else {
                    error(~stream=strm, Exn_CharToken(u));
                  };
                } else {
                  error(~stream=strm, Exn_CharToken(u));
                }
              );

            get_code(false, u - 48);
          } else {
            error(~stream=strm, Exn_CharToken(u));
          }
        );
      } else {
        aux_entity(u, entities);
      }
    );
  };

  let rec text_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (u == u_lt) {
        let txt = extract_buffer(state);
        state.next_state = LessThanSignState;
        X.emit_text(txt);
      } else if (u == u_amp) {
        character_reference(strm)
        >>= (
          u => {
            add_chars(state, E.encode_unicode(u));
            text_state(state, strm);
          }
        );
      } else {
        add_chars(state, E.encode_unicode(u));
        text_state(state, strm);
      }
    );

  let comment_state = (state, strm) => {
    let rec start = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (u == u_dash) {
          comment_dash_state(state, strm);
        } else {
          /* if need, add to state.tmp_buffer, but here we ignore comments */
          start(state, strm);
        }
      )
    and comment_dash_state = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (u == u_dash) {
          comment_dash_dash_state(state, strm);
        } else {
          /* if need, add to state.tmp_buffer, but here we ignore comments */
          start(
            state,
            strm,
          );
        }
      )
    and comment_dash_dash_state = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (u == u_gt) {
          /* end of comment */
          S.return();
        } else {
          error(~stream=strm, Exn_msg("Unexpected '--'"));
        }
      );

    start(state, strm);
  };

  let rec start_tag_name_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (is_space(u)) {
        let tagname = extract_buffer(state);
        before_attribute_state(tagname, [], state, strm);
      } else if (u == u_slash) {
        let tagname = extract_buffer(state);
        self_closing_start_tag_state(tagname, state, strm);
      } else if (u == u_gt) {
        let tagname = extract_buffer(state);
        Stack.push(tagname, state.stack);
        state.next_state = TextState;
        X.emit_start_tag(tagname, [], false);
      } else if (XName.is_name_char(u)) {
        add_chars(state, E.encode_unicode(u));
        start_tag_name_state(state, strm);
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    )
  and self_closing_start_tag_state = (tagname, state, strm) =>
    next_char(strm, not_eof, u =>
      if (u == u_gt) {
        if (Stack.is_empty(state.stack)) {
          state.next_state = AfterElement;
        } else {
          state.next_state = TextState;
        };
        X.emit_start_tag(tagname, [], true);
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    )
  and end_tag_start_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (XName.is_name_start_char(u)) {
        add_chars(state, E.encode_unicode(u));
        end_tag_state(state, strm);
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    )
  and end_tag_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (is_space(u)) {
        after_end_tag_state(state, strm);
      } else if (u == u_gt) {
        let tagname = extract_buffer(state);
        let name = Stack.pop(state.stack);
        if (name == tagname) {
          if (Stack.is_empty(state.stack)) {
            state.next_state = AfterElement;
          } else {
            state.next_state = TextState;
          };
          X.emit_end_tag(tagname);
        } else {
          error(~stream=strm, Exn_msg("Invalid end tag name"));
        };
      } else if (XName.is_name_char(u)) {
        add_chars(state, E.encode_unicode(u));
        end_tag_state(state, strm);
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    )
  and after_end_tag_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (is_space(u)) {
        after_end_tag_state(state, strm);
      } else if (u == u_gt) {
        let tagname = extract_buffer(state);
        let name = Stack.pop(state.stack);
        if (tagname == name) {
          if (Stack.is_empty(state.stack)) {
            state.next_state = AfterElement;
          } else {
            state.next_state = TextState;
          };
          X.emit_end_tag(tagname);
        } else {
          error(~stream=strm, Exn_msg("Invalid end tag name"));
        };
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    )
  and before_attribute_state = (tagname, attrs, state, strm) =>
    next_char(strm, not_eof, u =>
      if (is_space(u)) {
        before_attribute_state(tagname, attrs, state, strm);
      } else if (u == u_slash) {
        consume_sequence(strm, [u_gt])
        >>= (
          () => {
            state.next_state = TextState;
            X.emit_start_tag(tagname, attrs, true);
          }
        );
      } else if (u == u_gt) {
        Stack.push(tagname, state.stack);
        state.next_state = TextState;
        X.emit_start_tag(tagname, attrs, false);
      } else if (XName.is_name_start_char(u)) {
        add_chars(state, E.encode_unicode(u));
        attribute_name_state(tagname, attrs, state, strm);
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    )
  and attribute_name_state = (tagname, attrs, state, strm) =>
    next_char(strm, not_eof, u =>
      if (u == u_eq) {
        let name = extract_buffer(state);
        before_attribute_value_state(state, strm)
        >>= (
          value =>
            after_attribute_value_state(
              tagname,
              [(name, value), ...attrs],
              state,
              strm,
            )
        );
      } else if (is_space(u)) {
        let name = extract_buffer(state);
        after_attribute_name_state(state, strm)
        >>= (
          value =>
            after_attribute_value_state(
              tagname,
              [(name, value), ...attrs],
              state,
              strm,
            )
        );
      } else if (XName.is_name_char(u)) {
        add_chars(state, E.encode_unicode(u));
        attribute_name_state(tagname, attrs, state, strm);
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    )
  and after_attribute_name_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (is_space(u)) {
        after_attribute_name_state(state, strm);
      } else if (u == u_eq) {
        before_attribute_value_state(state, strm);
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    )
  and before_attribute_value_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (is_space(u)) {
        before_attribute_value_state(state, strm);
      } else if (u == u_quot) {
        attribute_value_double_quoted_state(state, strm);
      } else if (u == u_apos) {
        attribute_value_single_quoted_state(state, strm);
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    )
  and attribute_value_double_quoted_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (u == u_quot) {
        let value = extract_buffer(state);
        S.return(value);
      } else if (u == u_amp) {
        character_reference(strm)
        >>= (
          u => {
            add_chars(state, E.encode_unicode(u));
            attribute_value_double_quoted_state(state, strm);
          }
        );
      } else if (u == u_lt) {
        error(~stream=strm, Exn_CharToken(u));
      } else {
        add_chars(state, E.encode_unicode(u));
        attribute_value_double_quoted_state(state, strm);
      }
    )
  and attribute_value_single_quoted_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (u == u_apos) {
        let value = extract_buffer(state);
        S.return(value);
      } else if (u == u_amp) {
        character_reference(strm)
        >>= (
          u => {
            add_chars(state, E.encode_unicode(u));
            attribute_value_single_quoted_state(state, strm);
          }
        );
      } else if (u == u_lt) {
        error(~stream=strm, Exn_CharToken(u));
      } else {
        add_chars(state, E.encode_unicode(u));
        attribute_value_single_quoted_state(state, strm);
      }
    )
  and after_attribute_value_state = (tagname, attrs, state, strm) =>
    next_char(strm, not_eof, u =>
      if (is_space(u)) {
        before_attribute_state(tagname, attrs, state, strm);
      } else if (u == u_slash) {
        consume_sequence(strm, [u_gt])
        >>= (
          () => {
            if (Stack.is_empty(state.stack)) {
              state.next_state = AfterElement;
            } else {
              state.next_state = TextState;
            };
            X.emit_start_tag(tagname, attrs, true);
          }
        );
      } else if (u == u_gt) {
        Stack.push(tagname, state.stack);
        state.next_state = TextState;
        X.emit_start_tag(tagname, attrs, false);
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    );

  let rec pi_start_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (XName.is_name_start_char(u)) {
        add_chars(state, E.encode_unicode(u));
        pi_target_state(state, strm);
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    )
  and pi_target_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (is_space(u)) {
        let target = extract_buffer(state);
        before_pi_data_state(target, state, strm);
      } else if (u == u_qmark) {
        let target = extract_buffer(state);
        consume_sequence(strm, [u_gt]) >>= (() => S.return((target, "")));
      } else if (XName.is_name_char(u)) {
        add_chars(state, E.encode_unicode(u));
        pi_target_state(state, strm);
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    )
  and before_pi_data_state = (target, state, strm) =>
    next_char(strm, not_eof, u =>
      if (is_space(u)) {
        before_pi_data_state(target, state, strm);
      } else if (u == u_qmark) {
        pi_data_qmark_state(target, state, strm);
      } else {
        add_chars(state, E.encode_unicode(u));
        pi_data_state(target, state, strm);
      }
    )
  and pi_data_state = (target, state, strm) =>
    next_char(strm, not_eof, u =>
      if (u == u_qmark) {
        pi_data_qmark_state(target, state, strm);
      } else {
        add_chars(state, E.encode_unicode(u));
        pi_data_state(target, state, strm);
      }
    )
  and pi_data_qmark_state = (target, state, strm) =>
    next_char(strm, not_eof, u =>
      if (u == u_gt) {
        let data = extract_buffer(state);
        S.return((target, data));
      } else {
        add_chars(state, E.encode_unicode(u_qmark));
        add_chars(state, E.encode_unicode(u));
        pi_data_state(target, state, strm);
      }
    );

  let doctype_state = (state, strm) => {
    let rec start = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          start(state, strm);
        } else if (XName.is_name_start_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_name_state(state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_name_state = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (XName.is_name_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_name_state(state, strm);
        } else if (is_space(u)) {
          let name = extract_buffer(state);
          let doctype = {dtd_name: name, dtd_external_id: None, dtd: []};
          doctype_external_id_state(doctype, state, strm);
        } else if (u == u_gt) {
          let name = extract_buffer(state);
          let doctype = {dtd_name: name, dtd_external_id: None, dtd: []};
          X.emit_doctype(doctype);
        } else if (u == u_lbracket) {
          let name = extract_buffer(state);
          let doctype = {dtd_name: name, dtd_external_id: None, dtd: []};
          doctype_intsubsect_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_external_id_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_external_id_state(doctype, state, strm);
        } else if (u == u_gt) {
          X.emit_doctype(doctype);
        } else if (u == u_S) {
          consume_sequence(strm, [u_Y, u_S, u_T, u_E, u_M])
          >>= (
            () =>
              consume_space(strm)
              >>= (
                () =>
                  doctype_before_systemliteral_state(state, strm)
                  >>= (
                    systemid =>
                      doctype_before_intsubsect_state(
                        {
                          ...doctype,
                          dtd_external_id: Some(SystemID(systemid)),
                        },
                        state,
                        strm,
                      )
                  )
              )
          );
        } else if (u == u_P) {
          consume_sequence(strm, [u_U, u_B, u_L, u_I, u_C])
          >>= (
            () =>
              consume_space(strm)
              >>= (
                () =>
                  doctype_before_publicliteral_state(state, strm)
                  >>= (
                    ((public, system)) =>
                      doctype_before_intsubsect_state(
                        {
                          ...doctype,
                          dtd_external_id:
                            Some([@implicit_arity] PublicID(public, system)),
                        },
                        state,
                        strm,
                      )
                  )
              )
          );
        } else if (u == u_lbracket) {
          doctype_intsubsect_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_before_systemliteral_state = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_before_systemliteral_state(state, strm);
        } else if (u == u_quot || u == u_apos) {
          doctype_systemliteral_state(u, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_systemliteral_state = (lim, state, strm) =>
      next_char(strm, not_eof, u =>
        if (u == lim) {
          let value = extract_buffer(state);
          S.return(value);
        } else {
          add_chars(state, E.encode_unicode(u));
          doctype_systemliteral_state(lim, state, strm);
        }
      )
    and doctype_before_publicliteral_state = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_before_publicliteral_state(state, strm);
        } else if (u == u_quot || u == u_apos) {
          doctype_publicliteral_state(u, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_publicliteral_state = (lim, state, strm) =>
      next_char(strm, not_eof, u =>
        if (u == lim) {
          let value = extract_buffer(state);
          consume_space(strm)
          >>= (
            () =>
              doctype_before_systemliteral_state(state, strm)
              >>= (systemid => S.return((value, systemid)))
          );
        } else if (u == 32
                   || u == 13
                   || u == 10
                   || char_range(u, 97, 122)
                   || char_range(u, 65, 90)
                   || char_range(u, 48, 57)
                   || one_of(
                        u,
                        [
                          u_dash,
                          u_apos,
                          u_lparen,
                          u_rparen,
                          u_plus,
                          u_comma,
                          u_dot,
                          u_slash,
                          u_colon,
                          u_eq,
                          u_qmark,
                          u_semicolon,
                          u_excl,
                          u_star,
                          u_sharp,
                          u_at,
                          u_dollar,
                          u_underline,
                          u_percent,
                        ],
                      )) {
          add_chars(state, E.encode_unicode(u));
          doctype_publicliteral_state(lim, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_before_intsubsect_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_before_intsubsect_state(doctype, state, strm);
        } else if (u == u_gt) {
          X.emit_doctype(doctype);
        } else if (u == u_lbracket) {
          doctype_intsubsect_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_intsubsect_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_intsubsect_state(doctype, state, strm);
        } else if (u == u_rbracket) {
          doctype_intsubsect_end_state(doctype, state, strm);
        } else if (u == u_percent) {
          doctype_pereference_start_state(doctype, state, strm);
        } else if (u == u_lt) {
          doctype_markupdecl_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_intsubsect_end_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_intsubsect_end_state(doctype, state, strm);
        } else if (u == u_gt) {
          X.emit_doctype(doctype);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_pereference_start_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (XName.is_name_start_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_pereference_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_pereference_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (u == u_semicolon) {
          let name = extract_buffer(state);
          doctype_intsubsect_state(
            {...doctype, dtd: [DTD_PEReference(name), ...doctype.dtd]},
            state,
            strm,
          );
        } else if (XName.is_name_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_pereference_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_markupdecl_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (u == u_excl) {
          doctype_markupdecl_excl_state(doctype, state, strm);
        } else if (u == u_qmark) {
          pi_start_state(state, strm)
          >>= (
            ((target, data)) =>
              doctype_intsubsect_state(
                {
                  ...doctype,
                  dtd: [
                    [@implicit_arity] DTD_PI(target, data),
                    ...doctype.dtd,
                  ],
                },
                state,
                strm,
              )
          );
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_markupdecl_excl_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (u == u_dash) {
          consume_sequence(strm, [u_dash])
          >>= (
            () =>
              comment_state(state, strm)
              >>= (() => doctype_intsubsect_state(doctype, state, strm))
          );
        } else if (u == u_A) {
          consume_sequence(strm, [u_T, u_T, u_L, u_I, u_S, u_T])
          >>= (
            () =>
              consume_space(strm)
              >>= (() => doctype_attlist_state(doctype, state, strm))
          );
        } else if (u == u_E) {
          next_char(strm, not_eof, u =>
            if (u == u_N) {
              consume_sequence(strm, [u_T, u_I, u_T, u_Y])
              >>= (
                () =>
                  consume_space(strm)
                  >>= (() => doctype_entity_state(doctype, state, strm))
              );
            } else if (u == u_L) {
              consume_sequence(strm, [u_E, u_M, u_E, u_N, u_T])
              >>= (
                () =>
                  consume_space(strm)
                  >>= (() => doctype_element_state(doctype, state, strm))
              );
            } else {
              error(~stream=strm, Exn_CharToken(u));
            }
          );
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_attlist_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_attlist_state(doctype, state, strm);
        } else if (XName.is_name_start_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_attlist_name_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_attlist_name_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          let name = extract_buffer(state);
          doctype_attlist_attdef_state([], state, strm)
          >>= (
            defs =>
              doctype_intsubsect_state(
                {
                  ...doctype,
                  dtd: [
                    [@implicit_arity] DTD_ATTLIST(name, defs),
                    ...doctype.dtd,
                  ],
                },
                state,
                strm,
              )
          );
        } else if (XName.is_name_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_attlist_name_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_attlist_attdef_state = (defs, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_attlist_attdef_state(defs, state, strm);
        } else if (XName.is_name_start_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_attlist_attdef_name_state(state, strm)
          >>= (
            attdef =>
              doctype_after_attdef_state([attdef, ...defs], state, strm)
          );
        } else if (u == u_gt) {
          S.return(defs);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_attlist_attdef_name_state = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          let attrname = extract_buffer(state);
          doctype_attlist_atttype_state(state, strm)
          >>= (
            atttype =>
              doctype_attlist_defaultdecl(state, strm)
              >>= (defaultdecl => S.return((attrname, atttype, defaultdecl)))
          );
        } else if (XName.is_name_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_attlist_attdef_name_state(state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_attlist_atttype_state = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_attlist_atttype_state(state, strm);
        } else if (u == u_C) {
          consume_sequence(strm, [u_D, u_A, u_T, u_A])
          >>= (() => consume_space(strm) >>= (() => S.return(`CDATA)));
        } else if (u == u_I) {
          consume_sequence(strm, [u_D])
          >>= (() => consume_space(strm) >>= (() => S.return(`ID)));
        } else if (u == u_N) {
          consume_sequence(strm, [u_M, u_T, u_O, u_K, u_E, u_N])
          >>= (() => consume_space(strm) >>= (() => S.return(`NMTOKEN)));
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_attlist_defaultdecl = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_attlist_defaultdecl(state, strm);
        } else if (u == u_sharp) {
          next_char(strm, not_eof, u =>
            if (u == u_R) {
              consume_sequence(strm, [u_E, u_Q, u_U, u_I, u_R, u_E, u_D])
              >>= (() => S.return(`Required));
            } else if (u == u_I) {
              consume_sequence(strm, [u_M, u_P, u_L, u_I, u_E, u_D])
              >>= (() => S.return(`Implied));
            } else {
              error(~stream=strm, Exn_CharToken(u));
            }
          );
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_after_attdef_state = (defs, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_attlist_attdef_state(defs, state, strm);
        } else if (u == u_gt) {
          S.return(defs);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_entity_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_entity_state(doctype, state, strm);
        } else if (u == u_percent) {
          consume_space(strm)
          >>= (() => doctype_entity_pedecl_state(doctype, state, strm));
        } else if (XName.is_name_start_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_entity_gedecl_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_entity_pedecl_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_entity_pedecl_state(doctype, state, strm);
        } else if (XName.is_name_start_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_entity_pedecl_name_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_entity_pedecl_name_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          let name = extract_buffer(state);
          before_doctype_pedef_state(doctype, name, state, strm);
        } else if (XName.is_name_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_entity_pedecl_name_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and before_doctype_pedef_state = (doctype, name, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          before_doctype_pedef_state(doctype, name, state, strm);
        } else if (u == u_quot || u == u_apos) {
          doctype_entityvalue_state(u, state, strm)
          >>= (
            value =>
              doctype_markupdecl_end_state(
                {
                  ...doctype,
                  dtd: [
                    DTD_Entity(
                      [@implicit_arity] PEDecl(name, `EntityValue(value)),
                    ),
                    ...doctype.dtd,
                  ],
                },
                state,
                strm,
              )
          );
        } else if (u == u_S) {
          consume_sequence(strm, [u_Y, u_S, u_T, u_E, u_M])
          >>= (
            () =>
              consume_space(strm)
              >>= (
                () =>
                  doctype_before_systemliteral_state(state, strm)
                  >>= (
                    system =>
                      doctype_markupdecl_end_state(
                        {
                          ...doctype,
                          dtd: [
                            DTD_Entity(
                              [@implicit_arity]
                              PEDecl(name, `ExternalID(SystemID(system))),
                            ),
                            ...doctype.dtd,
                          ],
                        },
                        state,
                        strm,
                      )
                  )
              )
          );
        } else if (u == u_P) {
          consume_sequence(strm, [u_U, u_B, u_L, u_I, u_C])
          >>= (
            () =>
              consume_space(strm)
              >>= (
                () =>
                  doctype_before_publicliteral_state(state, strm)
                  >>= (
                    ((public, system)) =>
                      doctype_markupdecl_end_state(
                        {
                          ...doctype,
                          dtd: [
                            DTD_Entity(
                              [@implicit_arity]
                              PEDecl(
                                name,
                                `ExternalID(
                                  [@implicit_arity] PublicID(public, system),
                                ),
                              ),
                            ),
                            ...doctype.dtd,
                          ],
                        },
                        state,
                        strm,
                      )
                  )
              )
          );
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_entityvalue_state = (lim, state, strm) =>
      next_char(strm, not_eof, u =>
        if (u == lim) {
          let value = extract_buffer(state);
          S.return(value);
        } else {
          /* TODO: Pereference, Reference */
          add_chars(state, E.encode_unicode(u));
          doctype_entityvalue_state(lim, state, strm);
        }
      )
    and doctype_entity_gedecl_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          let name = extract_buffer(state);
          doctype_gedecl_entitydef_state(doctype, name, state, strm);
        } else if (XName.is_name_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_entity_gedecl_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_gedecl_entitydef_state = (doctype, name, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_gedecl_entitydef_state(doctype, name, state, strm);
        } else if (u == u_quot || u == u_apos) {
          doctype_entityvalue_state(u, state, strm)
          >>= (
            value =>
              doctype_markupdecl_end_state(
                {
                  ...doctype,
                  dtd: [
                    DTD_Entity(
                      [@implicit_arity] GEDecl(name, `EntityValue(value)),
                    ),
                    ...doctype.dtd,
                  ],
                },
                state,
                strm,
              )
          );
        } else if (u == u_S) {
          consume_sequence(strm, [u_Y, u_S, u_T, u_E, u_M])
          >>= (
            () =>
              consume_space(strm)
              >>= (
                () =>
                  doctype_before_systemliteral_state(state, strm)
                  >>= (
                    system =>
                      doctype_gedecl_notion_state(state, strm)
                      >>= (
                        notion =>
                          doctype_markupdecl_end_state(
                            {
                              ...doctype,
                              dtd: [
                                DTD_Entity(
                                  [@implicit_arity]
                                  GEDecl(
                                    name,
                                    `ExternalID((SystemID(system), notion)),
                                  ),
                                ),
                                ...doctype.dtd,
                              ],
                            },
                            state,
                            strm,
                          )
                      )
                  )
              )
          );
        } else if (u == u_P) {
          consume_sequence(strm, [u_U, u_B, u_L, u_I, u_C])
          >>= (
            () =>
              consume_space(strm)
              >>= (
                () =>
                  doctype_before_publicliteral_state(state, strm)
                  >>= (
                    ((public, system)) =>
                      doctype_gedecl_notion_state(state, strm)
                      >>= (
                        notion =>
                          doctype_markupdecl_end_state(
                            {
                              ...doctype,
                              dtd: [
                                DTD_Entity(
                                  [@implicit_arity]
                                  GEDecl(
                                    name,
                                    `ExternalID((
                                      [@implicit_arity]
                                      PublicID(public, system),
                                      notion,
                                    )),
                                  ),
                                ),
                                ...doctype.dtd,
                              ],
                            },
                            state,
                            strm,
                          )
                      )
                  )
              )
          );
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_gedecl_notion_state = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_gedecl_notion_space_state(state, strm);
        } else if (u == u_gt) {
          S.return(None);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_gedecl_notion_space_state = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_gedecl_notion_space_state(state, strm);
        } else if (u == u_gt) {
          S.return(None);
        } else if (u == u_N) {
          consume_sequence(strm, [u_D, u_A, u_T, u_A])
          >>= (
            () =>
              consume_space(strm)
              >>= (() => doctype_gedecl_notion_before_name_state(state, strm))
          );
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_gedecl_notion_before_name_state = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_gedecl_notion_before_name_state(state, strm);
        } else if (XName.is_name_start_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_gedecl_notion_name(state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_gedecl_notion_name = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          let name = extract_buffer(state);
          S.return(Some(name));
        } else if (XName.is_name_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_gedecl_notion_name(state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_element_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_element_state(doctype, state, strm);
        } else if (XName.is_name_start_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_element_name_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_element_name_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          let name = extract_buffer(state);
          doctype_element_contentspec_state(doctype, name, state, strm);
        } else if (XName.is_name_char(u)) {
          add_chars(state, E.encode_unicode(u));
          doctype_element_name_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_element_contentspec_state = (doctype, name, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_element_contentspec_state(doctype, name, state, strm);
        } else if (u == u_E) {
          consume_sequence(strm, [u_M, u_P, u_T, u_Y])
          >>= (
            () =>
              doctype_markupdecl_end_state(
                {
                  ...doctype,
                  dtd: [
                    [@implicit_arity] DTD_Element(name, `Empty),
                    ...doctype.dtd,
                  ],
                },
                state,
                strm,
              )
          );
        } else if (u == u_A) {
          consume_sequence(strm, [u_N, u_Y])
          >>= (
            () =>
              doctype_markupdecl_end_state(
                {
                  ...doctype,
                  dtd: [
                    [@implicit_arity] DTD_Element(name, `Any),
                    ...doctype.dtd,
                  ],
                },
                state,
                strm,
              )
          );
        } else {
          /* TODO */
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and doctype_markupdecl_end_state = (doctype, state, strm) =>
      next_char(strm, not_eof, u =>
        if (is_space(u)) {
          doctype_markupdecl_end_state(doctype, state, strm);
        } else if (u == u_gt) {
          doctype_intsubsect_state(doctype, state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      );

    start(state, strm);
  };

  let prolog_state = (state, strm) => {
    let rec start = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (u == u_lt) {
          prolog_less_than_sign_state(state, strm);
        } else if (is_space(u)) {
          state.next_state = PrologMiscState;
          start(state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and prolog_less_than_sign_state = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (u == u_qmark) {
          pi_start_state(state, strm)
          >>= (
            ((target, data)) =>
              if (target == "xml") {
                if (state.next_state == PrologXmlDeclState) {
                  let (_version, encoding, _standalone) =
                    parse_xmldecl(data);
                  switch (encoding) {
                  | Some(encname) => set_decoder(encname, strm)
                  | None => ()
                  };
                  state.next_state = PrologMiscState;
                  start(state, strm);
                } else {
                  error(~stream=strm, Exn_msg("Illegal PI target"));
                };
              } else {
                X.emit_pi(target, data);
              }
          );
        } else if (u == u_excl) {
          prolog_markup_state(state, strm);
        } else if (XName.is_name_start_char(u)) {
          state.next_state = LessThanSignState;
          add_chars(state, E.encode_unicode(u));
          start_tag_name_state(state, strm);
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      )
    and prolog_markup_state = (state, strm) =>
      next_char(strm, not_eof, u =>
        if (u == u_dash) {
          consume_sequence(strm, [u_dash])
          >>= (
            () => comment_state(state, strm) >>= (() => start(state, strm))
          );
        } else if (u == u_D) {
          consume_sequence(strm, [u_O, u_C, u_T, u_Y, u_P, u_E])
          >>= (
            () => consume_space(strm) >>= (() => doctype_state(state, strm))
          );
        } else {
          error(~stream=strm, Exn_CharToken(u));
        }
      );

    start(state, strm);
  };

  let rec cdata_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (u == u_rbracket) {
        cdata_rbracket_state(state, strm);
      } else {
        add_chars(state, E.encode_unicode(u));
        cdata_state(state, strm);
      }
    )
  and cdata_rbracket_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (u == u_rbracket) {
        cdata_rbracket_rbracket_state(state, strm);
      } else {
        add_chars(state, E.encode_unicode(u_rbracket));
        add_chars(state, E.encode_unicode(u));
        cdata_state(state, strm);
      }
    )
  and cdata_rbracket_rbracket_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (u == u_gt) {
        let txt = extract_buffer(state);
        state.next_state = TextState;
        X.emit_text(txt);
      } else {
        add_chars(state, E.encode_unicode(u_rbracket));
        add_chars(state, E.encode_unicode(u_rbracket));
        add_chars(state, E.encode_unicode(u));
        cdata_state(state, strm);
      }
    );

  let markup_start_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (u == u_lbracket) {
        consume_sequence(strm, [u_C, u_D, u_A, u_T, u_A, u_lbracket])
        >>= (() => cdata_state(state, strm));
      } else if (u == u_dash) {
        consume_sequence(strm, [u_dash])
        >>= (
          () =>
            comment_state(state, strm) >>= (() => text_state(state, strm))
        );
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    );

  let less_than_sign_state = (state, strm) =>
    next_char(strm, not_eof, u =>
      if (u == u_slash) {
        end_tag_start_state(state, strm);
      } else if (u == u_excl) {
        markup_start_state(state, strm);
      } else if (u == u_qmark) {
        pi_start_state(state, strm)
        >>= (((target, data)) => X.emit_pi(target, data));
      } else if (XName.is_name_start_char(u)) {
        add_chars(state, E.encode_unicode(u));
        start_tag_name_state(state, strm);
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    );

  let rec after_element_state = (state, strm) =>
    next_char(strm, X.emit_eof, u =>
      if (is_space(u)) {
        after_element_state(state, strm);
      } else if (u == u_lt) {
        next_char(strm, not_eof, u =>
          if (u == u_qmark) {
            pi_start_state(state, strm)
            >>= (((target, data)) => X.emit_pi(target, data));
          } else if (u == u_excl) {
            consume_sequence(strm, [u_dash, u_dash])
            >>= (
              () =>
                comment_state(state, strm)
                >>= (() => after_element_state(state, strm))
            );
          } else {
            error(~stream=strm, Exn_CharToken(u));
          }
        );
      } else {
        error(~stream=strm, Exn_CharToken(u));
      }
    );

  let tokenizer = (state, strm) =>
    switch (state.next_state) {
    | TextState => text_state(state, strm)
    | LessThanSignState => less_than_sign_state(state, strm)
    | AfterElement => after_element_state(state, strm)
    | PrologXmlDeclState => prolog_state(state, strm)
    | PrologMiscState => prolog_state(state, strm)
    };

  let create_state = () => {
    tmp_buffer: Buffer.create(30),
    stack: Stack.create(),
    next_state: PrologXmlDeclState,
  };

  let lexer = (state, strm) => tokenizer(state, strm);

  let reset = state => {
    Buffer.reset(state.tmp_buffer);
    Stack.clear(state.stack);
    state.next_state = PrologXmlDeclState;
  };

  let make_lexer = strm => {
    let state = create_state();
    () => tokenizer(state, strm);
  };
};
