/*
 * (c) 2008 Anastasia Gornostaeva
 */

open Xml;
open Xmlparser;

let print_result = data => print_endline("Yes!");

{
  let file = Sys.argv[1];
  let f_in = open_in(file);
  let buf = Buffer.create(8129);
  let str = String.create(1024);
  let rec read_file = () => {
    let size = input(f_in, str, 0, 1024);
    if (size == 0) {
      close_in(f_in);
      Buffer.contents(buf);
    } else {
      Buffer.add_string(buf, String.sub(str, 0, size));
      read_file();
    };
  };

  let data = read_file();
  let process_production = ((tag, state)) =>
    switch (tag) {
    | Doctype(dtd) => print_result(dtd.dtd_intsubset)
    | _ => failwith(string_of_production(tag))
    };

  process_production(parse_dtd(data));
};
