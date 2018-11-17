open Xmllexer;

module Input = {
  open Lwt;
  type t('a) = Lwt.t('a);

  type stream = Lwt_stream.t(char);
  let get = s =>
    Lwt_stream.peek(s)
    >>= (
      fun
      | Some(c) => Lwt_stream.junk(s) >>= (() => return(Some(c)))
      | None => return(None)
    );

  module D =
    Decoder({
      type t('a) = Lwt.t('a);
      let fail = Lwt.fail;
      let return = Lwt.return;
      let (>>=) = Lwt.(>>=);
      type stream = Lwt_stream.t(char);
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

module LS = Xmllexer.LocatedStream(Lwt, Input);
module M =
  Xmllexer_generic.Make(LS, Xmllexer.Encoding, (Xmllexer.XmlStanza(Lwt)));

let (>>=) = Lwt.(>>=);

Lwt_main.run(
  Lwt_io.open_file(~mode=Lwt_io.input, Sys.argv[1])
  >>= (
    f => {
      let strm = Lwt_io.read_chars(f);
      let strm = LS.make_stream(strm);
      let next_token = M.make_lexer(strm);
      let rec loop = () =>
        next_token()
        >>= (
          fun
          | Some(_) => loop()
          | None => Lwt.return()
        );

      loop();
    }
  ),
);
