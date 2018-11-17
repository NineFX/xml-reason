/*
 * (c) 2007-2012 Anastasia Gornostaeva
 */

open Xmllexer_generic;

module IterMonad = {
  type input = {
    buf: bytes,
    i: int,
    len: int,
  };

  type data =
    | Chunk(input)
    | EOF;

  let empty_chunk = {buf: Bytes.create(0), i: 0, len: 0};

  let empty_stream = Chunk(empty_chunk);

  let is_empty = s => s.i == s.len;

  type t('a) =
    | Return('a)
    | Continue(data => (t('a), data));

  let return = x => Return(x);
  let fail = raise;
  let rec bind = (v, f) =>
    switch (v) {
    | Return(x) => f(x)
    | Continue(k) =>
      Continue(
        (
          s =>
            switch (k(s)) {
            | (Return(x), s') =>
              switch (f(x)) {
              | Continue(k) => k(s')
              | i => (i, s')
              }
            | (i, s') => (bind(i, f), s')
            }
        ),
      )
    };

  let (>>=) = bind;

  let rec get =
    Continue(
      fun
      | EOF => (Return(None), EOF)
      | Chunk(s) =>
        if (s.i < s.len) {
          (
            Return(Some(Bytes.get(s.buf, s.i))),
            Chunk({...s, i: s.i + 1}),
          );
        } else {
          (get, Chunk(s));
        },
    );
};

module Decoder =
       (
         I: {
           type t('a);
           let (>>=): (t('a), 'a => t('b)) => t('b);
           let return: 'a => t('a);
           let fail: exn => t('a);
           type stream;
           let get: t(option(char));
         },
       ) => {
  open I;

  exception IllegalCharacter;

  let decode_utf8 =
    I.get
    >>= (
      fun
      | None => return(None)
      | Some(ch1) =>
        switch (ch1) {
        | '\000'..'\127' => return(Some(Char.code(ch1)))
        | 'À'..'ß' =>
          I.get
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
          I.get
          >>= (
            fun
            | None => fail(IllegalCharacter)
            | Some(ch2) =>
              I.get
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
          I.get
          >>= (
            fun
            | None => fail(IllegalCharacter)
            | Some(ch2) =>
              I.get
              >>= (
                fun
                | None => fail(IllegalCharacter)
                | Some(ch3) =>
                  I.get
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

module Input = {
  module D =
    Decoder({
      include IterMonad;
      type stream = IterMonad.data;
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

module LocatedStream = {
  include IterMonad;

  exception Located_exn((int, int), exn);

  type stream = {
    mutable line: int,
    mutable col: int,
    mutable decoder: t(option(int)),
  };

  open Input;

  let set_decoder = (encname, strm) => {
    let decoder = make_decoder(encname);
    strm.decoder = decoder;
    ();
  };

  type source = unit;

  let make_stream = () => {line: 0, col: 0, decoder: make_decoder("UTF-8")};

  let error = (~stream=?, exn) =>
    switch (stream) {
    | None => fail(exn)
    | Some(strm) =>
      fail([@implicit_arity] Located_exn((strm.line, strm.col), exn))
    };

  let next_char = (strm, eof, f) =>
    strm.decoder
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

module X = XmlStanza(IterMonad);

module M = Make(LocatedStream, Encoding, X);

open IterMonad;

let parse_document = inc => {
  let stream = LocatedStream.make_stream();
  let buf = String.create(8192);
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
  and loop = v =>
    switch (v) {
    | (Return(Some(result)), s) => loop((process_token(result), s))
    | (Return(None), s) => ()
    | (Continue(cont), s) =>
      let s =
        switch (s) {
        | EOF => EOF
        | Chunk(s) =>
          if (is_empty(s)) {
            let size = input(inc, buf, 0, 8192);
            if (size == 0) {
              EOF;
            } else {
              let str =
                if (size < 8192) {
                  Bytes.sub(buf, 0, size);
                } else {
                  buf;
                };

              Chunk({buf: str, i: 0, len: size});
            };
          } else {
            Chunk(s);
          }
        };

      loop(cont(s));
    };

  try (
    {
      loop((next_token(), empty_stream));
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
