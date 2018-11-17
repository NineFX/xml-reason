/*
 * (c) 2007-2012 Anastasia Gornostaeva
 */

open Xmllexer_generic;

module IterMonad = {
  type input = {
    buf: bytes,
    mutable i: int,
    mutable len: int,
    mutable is_final: bool,
  };

  let make_chunk = size => {
    buf: Bytes.create(size),
    i: 0,
    len: 0,
    is_final: false,
  };

  let is_empty = s => s.i == s.len;

  type t('a) =
    | Return('a)
    | Continue(input => t('a));

  let return = x => Return(x);
  let fail = raise;
  let rec bind = (v, f) =>
    switch (v) {
    | Return(x) => f(x)
    | Continue(cont) =>
      Continue(
        (
          s =>
            switch (cont(s)) {
            | Return(x) => f(x)
            | i => bind(i, f)
            }
        ),
      )
    };

  let (>>=) = bind;

  exception IllegalCharacter;

  let rec get = s =>
    if (s.is_final) {
      Return(None);
    } else if (s.i < s.len) {
      let ch1 = Bytes.get(s.buf, s.i);
      s.i = s.i + 1;
      switch (ch1) {
      | '\000'..'\127' => Return(Some(Char.code(ch1)))
      | 'À'..'ß' =>
        let rec cont = s =>
          if (s.is_final) {
            fail(IllegalCharacter);
          } else if (s.i < s.len) {
            let ch2 = Bytes.get(s.buf, s.i);
            s.i = s.i + 1;
            let n1 = Char.code(ch1);
            let n2 = Char.code(ch2);
            if (n2 lsr 6 !== 2) {
              fail(IllegalCharacter);
            } else {
              let code = (n1 land 31) lsl 6 lor (n2 land 63);
              Return(Some(code));
            };
          } else {
            Continue(cont);
          };

        cont(s);

      | 'à'..'ï' =>
        let rec cont = s =>
          if (s.is_final) {
            fail(IllegalCharacter);
          } else if (s.i < s.len) {
            let ch2 = Bytes.get(s.buf, s.i);
            s.i = s.i + 1;
            let rec cont2 = s =>
              if (s.is_final) {
                fail(IllegalCharacter);
              } else if (s.i < s.len) {
                let ch3 = Bytes.get(s.buf, s.i);
                s.i = s.i + 1;
                let n1 = Char.code(ch1)
                and n2 = Char.code(ch2)
                and n3 = Char.code(ch3);
                if (n2 lsr 6 !== 2 || n3 lsr 6 !== 2) {
                  fail(IllegalCharacter);
                } else {
                  let code =
                    (n1 land 15) lsl 12 lor (n2 land 63) lsl 6 lor (n3 land 63);

                  if (code >= 55296 && code <= 57088) {
                    fail(IllegalCharacter);
                  } else {
                    Return(Some(code));
                  };
                };
              } else {
                Continue(cont2);
              };

            cont2(s);
          } else {
            Continue(cont);
          };

        cont(s);

      | 'ð'..'÷' =>
        let rec cont = s =>
          if (s.is_final) {
            fail(IllegalCharacter);
          } else if (s.i < s.len) {
            let ch2 = Bytes.get(s.buf, s.i);
            s.i = s.i + 1;
            let rec cont2 = s =>
              if (s.is_final) {
                fail(IllegalCharacter);
              } else if (s.i < s.len) {
                let ch3 = Bytes.get(s.buf, s.i);
                s.i = s.i + 1;
                let rec cont3 = s =>
                  if (s.is_final) {
                    fail(IllegalCharacter);
                  } else if (s.i < s.len) {
                    let ch4 = Bytes.get(s.buf, s.i);
                    s.i = s.i + 1;
                    let n1 = Char.code(ch1)
                    and n2 = Char.code(ch2)
                    and n3 = Char.code(ch3)
                    and n4 = Char.code(ch4);
                    if (n2 lsr 6 !== 2 || n3 lsr 6 !== 2 || n4 lsr 6 !== 2) {
                      fail(IllegalCharacter);
                    } else {
                      Return(
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
                  } else {
                    Continue(cont3);
                  };

                cont3(s);
              } else {
                Continue(cont2);
              };

            cont2(s);
          } else {
            Continue(cont);
          };

        cont(s);

      | _ => fail(IllegalCharacter)
      };
    } else {
      Continue(get);
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

module LocatedStream = {
  include IterMonad;

  exception Located_exn((int, int), exn);

  type stream = {
    mutable line: int,
    mutable col: int,
    decoder: IterMonad.input => t(option(int)),
  };

  let set_decoder = (encname, strm) => ();

  let make_stream = () => {line: 0, col: 0, decoder: get};

  let error = (~stream=?, exn) =>
    switch (stream) {
    | None => fail(exn)
    | Some(strm) =>
      fail([@implicit_arity] Located_exn((strm.line, strm.col), exn))
    };

  let next_char = (strm, eof, f) =>
    Continue(
      source =>
        strm.decoder(source)
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
        ),
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

module X = XmlStanza(IterMonad);

module M = Make(LocatedStream, Encoding, X);

open IterMonad;

let parse_document = inc => {
  let source = make_chunk(8192);
  let stream = LocatedStream.make_stream();
  let next_token = M.make_lexer(stream);
  let namespaces = Hashtbl.create(1);
  let () = Hashtbl.add(namespaces, "xml", Xml.ns_xml);
  let stack = Stack.create();
  let stack_ns = Stack.create();
  let add_element = el => {
    let (qname, attrs, subels) = Stack.pop(stack);
    Stack.push((qname, attrs, [el, ...subels]), stack);
  };

  let rec process_token =
    fun
    | [@implicit_arity] X.StartTag(name, attrs, selfclosing) => {
        let (qname, lnss, attrs) =
          Xml.parse_element_head(namespaces, name, attrs);
        let el = (qname, attrs, []);
        if (selfclosing) {
          Xml.remove_namespaces(namespaces, lnss);
          if (Stack.is_empty(stack)) {
            Stack.push(el, stack);
            next_token();
          } else {
            add_element(Xml.Xmlelement(el));
            Xml.remove_namespaces(namespaces, lnss);
            next_token();
          };
        } else {
          Stack.push(el, stack);
          Stack.push(lnss, stack_ns);
          next_token();
        };
      }
    | X.EndTag(_name) => {
        let lnss = Stack.pop(stack_ns);
        Xml.remove_namespaces(namespaces, lnss);
        if (Stack.length(stack) > 1) {
          let (q, a, els) = Stack.pop(stack);
          add_element(
            [@implicit_arity] Xml.Xmlelement(q, a, List.rev(els)),
          );
          next_token();
        } else {
          next_token();
        };
      }
    | X.Text(text) => {
        add_element(Xml.Xmlcdata(text));
        next_token();
      }
    | X.Doctype(_)
    | X.PI(_) => next_token()
  and loop =
    fun
    | Return(Some(result)) => loop(process_token(result))
    | Return(None) => ()
    | Continue(cont) =>
      if (source.i < source.len) {
        loop(cont(source));
      } else {
        let size = input(inc, source.buf, 0, 8192);
        if (size == 0) {
          source.is_final = true;
        } else {
          source.i = 0;
          source.len = size;
        };
        loop(cont(source));
      };

  try (
    {
      loop(next_token());
      let (q, a, els) = Stack.pop(stack);
      [@implicit_arity] Xml.Xmlelement(q, a, List.rev(els));
    }
  ) {
  | [@implicit_arity] LocatedStream.Located_exn((line, col), exn) =>
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
