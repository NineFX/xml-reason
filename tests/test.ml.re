let () = {
  let t = Sys.argv[1];
  let f = open_in(Sys.argv[2]);
  let xml =
    switch (t) {
    | "s" => Xmllexer.parse_document(f)
    | "i" => XmllexerI.parse_document(f)
    | "i2" => XmllexerI2.parse_document(f)
    | "e" => XmllexerE.parse_document(f)
    | _ => failwith("unknown option")
    };

  let ser = Xml.Serialization.create([]);
  let out = print_string;
  Xml.Serialization.serialize_document(ser, out, xml);
};
