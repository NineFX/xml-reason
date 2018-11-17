/*
 * (c) 2007-2012 Anastasia Gornostaeva
 */

open Xmllexer_generic;

module type INPUT = {
  type t('a);
  type stream;
  let make_decoder: (string, stream) => t(option(int));
};

module UnitMonad = {
  type t('a) = 'a;
  let return = x => x;
  let fail = raise;
  let (>>=) = (v, f) => f(v);
};

module Decoder =
       (
         I: {
           type t('a);
           let (>>=): (t('a), 'a => t('b)) => t('b);
           let return: 'a => t('a);
           let fail: exn => t('a);
           type stream;
           let get: stream => t(option(char));
         },
       ) => {
  open I;

  exception IllegalCharacter;

  let decode_utf8 = strm =>
    I.get(strm)
    >>= (
      fun
      | None => return(None)
      | Some(ch1) =>
        switch (ch1) {
        | '\000'..'\127' => return(Some(Char.code(ch1)))
        | 'À'..'ß' =>
          I.get(strm)
          >>= (
            fun
            | None => fail(IllegalCharacter)
            | Some(ch2) => {
                let n1 = Char.code(ch1);
                let n2 = Char.code(ch2);
                if (n2 lsr 6 !== 2) {
                  fail(IllegalCharacter);
                } else {
                  let code = (n1 land 31) lsl 6 lor (n2 land 63);
                  return(Some(code));
                };
              }
          )
        | 'à'..'ï' =>
          I.get(strm)
          >>= (
            fun
            | None => fail(IllegalCharacter)
            | Some(ch2) =>
              I.get(strm)
              >>= (
                fun
                | None => fail(IllegalCharacter)
                | Some(ch3) => {
                    let n1 = Char.code(ch1)
                    and n2 = Char.code(ch2)
                    and n3 = Char.code(ch3);
                    if (n2 lsr 6 !== 2 || n3 lsr 6 !== 2) {
                      fail(IllegalCharacter);
                    } else {
                      let code =
                        (n1 land 15)
                        lsl 12
                        lor (n2 land 63)
                        lsl 6
                        lor (n3 land 63);

                      if (code >= 55296 && code <= 57088) {
                        fail(IllegalCharacter);
                      } else {
                        return(Some(code));
                      };
                    };
                  }
              )
          )
        | 'ð'..'÷' =>
          I.get(strm)
          >>= (
            fun
            | None => fail(IllegalCharacter)
            | Some(ch2) =>
              I.get(strm)
              >>= (
                fun
                | None => fail(IllegalCharacter)
                | Some(ch3) =>
                  I.get(strm)
                  >>= (
                    fun
                    | None => fail(IllegalCharacter)
                    | Some(ch4) => {
                        let n1 = Char.code(ch1)
                        and n2 = Char.code(ch2)
                        and n3 = Char.code(ch3)
                        and n4 = Char.code(ch4);
                        if (n2 lsr 6 !== 2 || n3 lsr 6 !== 2 || n4 lsr 6 !== 2) {
                          fail(IllegalCharacter);
                        } else {
                          return(
                            Some(
                              (n1 land 7)
                              lsl 18
                              lor (n2 land 63)
                              lsl 12
                              lor (n3 land 63)
                              lsl 6
                              lor (n4 land 63),
                            ),
                          );
                        };
                      }
                  )
              )
          )
        | _ => fail(IllegalCharacter)
        }
    );
};

module Input = (M: MONAD) => {
  type t('a) = M.t('a);
  type stream = Stream.t(char);
  let get = s =>
    switch (Stream.peek(s)) {
    | Some(c) =>
      Stream.junk(s);
      M.return(Some(c));
    | None => M.return(None)
    };

  module D =
    Decoder({
      include M;
      type stream = Stream.t(char);
      let get = get;
    });

  exception UnknownEncoding;

  let make_decoder = encname =>
    if (encname == "UTF-8") {
      D.decode_utf8;
    } else {
      raise(UnknownEncoding);
    };
};

module Encoding = {
  exception IllegalCharacter;

  let encode_unicode = ucs4 => {
    let bytes =
      if (ucs4 < 128) {
        [ucs4];
      } else if (ucs4 <= 2047) {
        [192 lor ucs4 lsr 6, 128 lor (ucs4 land 63)];
      } else if (ucs4 <= 65535) {
        if (ucs4 >= 55296 & ucs4 < 57344) {
          raise(IllegalCharacter);
        };
        [
          224 lor ucs4 lsr 12,
          128 lor (ucs4 lsr 6 land 63),
          128 lor (ucs4 land 63),
        ];
      } else if (ucs4 <= 1114111) {
        [
          240 lor ucs4 lsr 18,
          128 lor (ucs4 lsr 12 land 63),
          128 lor (ucs4 lsr 6 land 63),
          128 lor (ucs4 land 63),
        ];
      } else {
        raise(IllegalCharacter);
      };

    List.map(Char.chr, bytes);
  };
};

module LocatedStream = (M: MONAD, I: INPUT with type t('a) = M.t('a)) => {
  include M;
  open I;

  type source = I.stream;

  exception Located_exn((int, int), exn);

  type stream = {
    mutable line: int,
    mutable col: int,
    mutable decoder: source => t(option(int)),
    stream: source,
  };

  let set_decoder = (encname, strm) => {
    let decoder = I.make_decoder(encname);
    strm.decoder = decoder;
    ();
  };

  let make_stream = source => {
    line: 0,
    col: 0,
    decoder: I.make_decoder("UTF-8"),
    stream: source,
  };

  let error = (~stream=?, exn) =>
    switch (stream) {
    | None => fail(exn)
    | Some(strm) =>
      M.fail([@implicit_arity] Located_exn((strm.line, strm.col), exn))
    };

  let next_char = (strm, eof, f) =>
    strm.decoder(strm.stream)
    >>= (
      fun
      | Some(u) => {
          if (u == 10) {
            strm.line = strm.line + 1;
            strm.col = 0;
          } else {
            strm.col = strm.col + 1;
          };
          f(u);
        }
      | None => eof()
    );
};

module XmlStanza = (M: MONAD) => {
  type data =
    | StartTag(string, list((string, string)), bool)
    | EndTag(string)
    | Doctype(doctype)
    | PI(string, string)
    | Text(string);

  type token = option(data);
  type t('a) = M.t('a);

  let emit_start_tag = (name, attrs, selfclosing) =>
    M.return(Some([@implicit_arity] StartTag(name, attrs, selfclosing)));

  let emit_end_tag = name => M.return(Some(EndTag(name)));

  let emit_doctype = doctype => M.return(Some(Doctype(doctype)));

  let emit_pi = (target, data) =>
    M.return(Some([@implicit_arity] PI(target, data)));

  let emit_text = text => M.return(Some(Text(text)));

  let emit_eof = () => M.return(None);
};

module LS = LocatedStream(UnitMonad, (Input(UnitMonad)));
module M = Make(LS, Encoding, (XmlStanza(UnitMonad)));

module X = XmlStanza(UnitMonad);
open Xml;

let parse_document = inc => {
  let strm = LS.make_stream(Stream.of_channel(inc));
  let next_token = M.make_lexer(strm);
  let namespaces = Hashtbl.create(1);
  let () = Hashtbl.add(namespaces, "xml", ns_xml);
  let stack = Stack.create();
  let add_element = el => {
    let (qname, attrs, subels) = Stack.pop(stack);
    Stack.push((qname, attrs, [el, ...subels]), stack);
  };

  let rec loop = () =>
    switch (next_token()) {
    | Some(t) =>
      switch (t) {
      | [@implicit_arity] X.StartTag(name, attrs, selfclosing) =>
        let (qname, lnss, attrs) =
          parse_element_head(namespaces, name, attrs);
        let el = (qname, attrs, []);
        if (selfclosing) {
          remove_namespaces(namespaces, lnss);
          if (Stack.is_empty(stack)) {
            Stack.push(el, stack);
            loop();
          } else {
            add_element(Xmlelement(el));
            loop();
          };
        } else {
          Stack.push(el, stack);
          loop();
          remove_namespaces(namespaces, lnss);
          loop();
        };
      | X.EndTag(_name) =>
        /* let qname = parse_qname namespaces (split_name name) in */
        if (Stack.length(stack) > 1) {
          let (q, a, els) = Stack.pop(stack);
          add_element([@implicit_arity] Xmlelement(q, a, List.rev(els)));
        } else {
          ();
        }
      | X.Text(text) =>
        add_element(Xmlcdata(text));
        loop();
      | X.Doctype(_)
      | X.PI(_) => loop()
      }
    | None => ()
    };

  try (
    {
      loop();
      let (q, a, els) = Stack.pop(stack);
      [@implicit_arity] Xmlelement(q, a, List.rev(els));
    }
  ) {
  | [@implicit_arity] LS.Located_exn((line, col), exn) =>
    switch (exn) {
    | M.Exn_msg(msg) =>
      Printf.eprintf("%d:%d %s\n", line, col, msg);
      Pervasives.exit(127);
    | M.Exn_ExpectedChar(chs) =>
      Printf.eprintf(
        "%d:%d Expected '%s'\n",
        line,
        col,
        String.make(1, List.hd(chs)),
      );
      Pervasives.exit(127);
    | M.Exn_CharToken(u) =>
      let chs = M.E.encode_unicode(u);
      let str = String.create(List.length(chs));
      let rec iteri = i => (
        fun
        | [] => ()
        | [x, ...xs] => {
            str.[i] = x;
            iteri(succ(i), xs);
          }
      );

      iteri(0, chs);
      Printf.eprintf(
        "%d:%d Unexpected character token %S\n",
        line,
        col,
        Bytes.to_string(str),
      );
      Pervasives.exit(127);
    | exn =>
      Printf.eprintf("%d:%d %s\n", line, col, Printexc.to_string(exn));
      Pervasives.exit(127);
    }
  };
};
