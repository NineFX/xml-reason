/*
 * (c) 2007-2012 Anastasia Gornostaeva
 */

open Xmllexer;

module XmlStanza = {
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

module LS = LocatedStream(UnitMonad, (Input(UnitMonad)));
module M = Xmllexer_generic.Make(LS, Encoding, XmlStanza);

{
  let strm = Stream.of_channel(open_in(Sys.argv[1]));
  let strm = LS.make_stream(strm);
  let next_token = M.make_lexer(strm);
  let rec loop = () => {
    next_token();
    loop();
  };
  loop();
};
