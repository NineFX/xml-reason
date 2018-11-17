/*
 * (c) 2007-2012 Anastasia Gornostaeva
 */

module StringStream = {
  type t('a) = 'a;
  let return = x => x;
  let fail = raise;

  type stream = {
    mutable i: int,
    len: int,
    buf: string,
  };

  let of_string = str => {buf: str, len: String.length(str), i: 0};

  let get = s =>
    if (s.i < s.len) {
      let c = s.buf.[s.i];
      s.i = s.i + 1;
      Some(c);
    } else {
      None;
    };

  open Encoding;
  exception IllegalCharacter;

  let make_decoder = encname => {
    let decoder = decoder(encname);
    strm =>
      if (strm.i < strm.len) {
        switch (decode(decoder, strm.buf, strm.i, strm.len - strm.i)) {
        | [@implicit_arity] Dec_ok(ucs4, j) =>
          strm.i = strm.i + j;
          return(Some(ucs4));
        | Dec_need_more => fail(IllegalCharacter)
        | Dec_error => fail(IllegalCharacter)
        };
      } else {
        return(None);
      };
  };
};

module XStanza = {
  type t('a) = 'a;

  type token = unit;

  let emit_start_tag = (name, attrs, selfclosing) => {
    Printf.printf("<%s", name);
    List.iter(((k, v)) => Printf.printf(" %s='%s'", k, v), attrs);
    Printf.printf(">");
    if (selfclosing) {
      Printf.printf("</%s>", name);
    };
  };

  let emit_end_tag = name => Printf.printf("</%s>", name);

  let emit_doctype = doctype => ();

  let emit_pi = (target, data) => Printf.printf("<?%s %s?>", target, data);

  let emit_text = text => Printf.printf("%s", text);

  let emit_eof = () => raise(End_of_file);
};

open Xmllexer;

module LS = LocatedStream(UnitMonad, StringStream);
module M = Xmllexer_generic.Make(LS, Encoding, XStanza);

{
  let f = open_in(Sys.argv[1]);
  let rec read_file = () => {
    let line =
      try (Some(input_line(f))) {
      | End_of_file => None
      };
    switch (line) {
    | Some(line) => line ++ "\n" ++ read_file()
    | None => ""
    };
  };

  let content = read_file();
  let strm = StringStream.of_string(content);
  let strm = LS.make_stream(strm);
  let next_token = M.make_lexer(strm);
  let rec loop = () => {
    next_token();
    loop();
  };
  let _ = loop();
  ();
};
