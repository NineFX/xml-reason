/*
 * (c) 2004-2012 Anastasia Gornostaeva
 */

type element =
  | Xmlelement((string, list((string, string)), list(element)))
  | Xmlcdata(string);

exception NonXmlelement;
exception Expected(string);

let decode = Xml_decode.decode;
let encode = Xml_encode.encode;

let rec attrs_to_string = attrs => {
  let attr_to_string = attr =>
    switch (attr) {
    | (name, value) => Printf.sprintf(" %s='%s'", name, encode(value))
    };
  List.fold_left((++), "", List.map(attr_to_string, attrs));
};

let rec element_to_string = el =>
  switch (el) {
  | [@implicit_arity] Xmlelement(name, attrs, els) =>
    if (List.length(els) > 0) {
      Printf.sprintf("<%s", name)
      ++ attrs_to_string(attrs)
      ++ ">"
      ++ List.fold_left((++), "", List.map(element_to_string, els))
      ++ Printf.sprintf("</%s>", name);
    } else {
      Printf.sprintf("<%s", name) ++ attrs_to_string(attrs) ++ "/>";
    }
  | Xmlcdata(chunk) => encode(chunk)
  };

let rec get_tag = (el: element, path: list(string)) =>
  switch (el) {
  | [@implicit_arity] Xmlelement(_, _, els) =>
    if (path == []) {
      el;
    } else {
      let name = List.hd(path);
      let ctag =
        List.find(
          fun
          | [@implicit_arity] Xmlelement(name1, _, _) => name == name1
          | Xmlcdata(_) => false,
          els,
        );
      get_tag(ctag, List.tl(path));
    }
  | Xmlcdata(_) => raise(NonXmlelement)
  };

let get_tag_full_path = (el, path) =>
  switch (el) {
  | [@implicit_arity] Xmlelement(tag, _, _) =>
    if (tag == List.hd(path)) {
      get_tag(el, List.tl(path));
    } else {
      raise(Not_found);
    }
  | Xmlcdata(_cdata) => raise(NonXmlelement)
  };

let get_subel = (~path=[], el) =>
  switch (get_tag(el, path)) {
  | [@implicit_arity] Xmlelement(_, _, els) =>
    List.find(
      fun
      | [@implicit_arity] Xmlelement(_, _, _) => true
      | Xmlcdata(_) => false,
      els,
    )
  | Xmlcdata(_) => raise(NonXmlelement)
  };

let get_subels = (~path=[], ~tag="", el) =>
  switch (get_tag(el, path)) {
  | [@implicit_arity] Xmlelement(_, _, els) =>
    if (tag == "") {
      els;
    } else if (els == []) {
      [];
    } else {
      List.find_all(
        fun
        | x =>
          switch (x) {
          | [@implicit_arity] Xmlelement(tag1, _, _) => tag1 == tag
          | Xmlcdata(_) => false
          },
        els,
      );
    }
  | Xmlcdata(_) => raise(NonXmlelement)
  };

let get_attr_s = (el, ~path=[], attrname: string) =>
  switch (get_tag(el, path)) {
  | [@implicit_arity] Xmlelement(_, attrs, _) => List.assoc(attrname, attrs)
  | Xmlcdata(_) => raise(NonXmlelement)
  };

let filter_attrs = attrs => {
  let checker = ((_k, v)) =>
    if (v == "") {
      false;
    } else {
      true;
    };
  List.filter(checker, attrs);
};

let rec collect_cdata = (els, acc) =>
  switch (els) {
  | [] => String.concat("", List.rev(acc))
  | [Xmlcdata(cdata), ...l] => collect_cdata(l, [cdata, ...acc])
  | [Xmlelement(_), ...l] => collect_cdata(l, acc)
  };

let get_cdata = (~path=[], el) =>
  switch (get_tag(el, path)) {
  | [@implicit_arity] Xmlelement(_, _, els) => collect_cdata(els, [])
  | Xmlcdata(_) => raise(NonXmlelement)
  };

let make_element = (name, attrs, els) =>
  [@implicit_arity] Xmlelement(name, attrs, els);

let make_simple_cdata = (name, cdata) =>
  [@implicit_arity] Xmlelement(name, [], [Xmlcdata(cdata)]);

let safe_get_attr_s = (xml, ~path=[], attrname) =>
  try (get_attr_s(xml, ~path, attrname)) {
  | _ => ""
  };

let match_tag = (tag, element) => {
  let b =
    switch (element) {
    | [@implicit_arity] Xmlelement(tag1, _, _) => tag1 == tag
    | Xmlcdata(_) => false
    };

  if (!b) {
    raise(Expected(tag));
  };
};

let exists_element = (tag, els) =>
  List.exists(
    fun
    | [@implicit_arity] Xmlelement(tag1, _, _) => tag1 == tag
    | Xmlcdata(_) => false,
    els,
  );

let find_subtag = (subels: list(element), tag: string) =>
  List.find(
    fun
    | [@implicit_arity] Xmlelement(tag1, _, _) => tag1 == tag
    | Xmlcdata(_) => false,
    subels,
  );

let get_tagname = el =>
  switch (el) {
  | [@implicit_arity] Xmlelement(name, _, _) => name
  | Xmlcdata(_) => raise(NonXmlelement)
  };

let match_xml = (el, tag, attrs: list((string, string))) =>
  switch (el) {
  | [@implicit_arity] Xmlelement(name, _, _) =>
    if (name == tag) {
      try (
        {
          List.iter(
            ((a, v)) =>
              if (get_attr_s(el, a) != v) {
                raise(Not_found);
              },
            attrs,
          );
          true;
        }
      ) {
      | _ => false
      };
    } else {
      false;
    }
  | Xmlcdata(_) => false
  };

let mem_xml = (xml, path, tag, attrs) =>
  if (get_tagname(xml) != List.hd(path)) {
    false;
  } else {
    try (
      {
        let els = get_subels(xml, ~path=List.tl(path), ~tag);
        List.exists(
          el =>
            try (
              {
                List.iter(
                  ((a, v)) =>
                    if (get_attr_s(el, a) != v) {
                      raise(Not_found);
                    },
                  attrs,
                );
                true;
              }
            ) {
            | _ => false
            },
          els,
        );
      }
    ) {
    | _ => false
    };
  };

let get_by_xmlns = (xml, ~path=?, ~tag=?, xmlns) => {
  let els = get_subels(xml, ~path?, ~tag?);
  List.find(
    x =>
      if (safe_get_attr_s(x, "xmlns") == xmlns) {
        true;
      } else {
        false;
      },
    els,
  );
};

open Xmllexer;
module XmlParser = M;
module X = XmlStanza(UnitMonad);
module S = LocatedStream(UnitMonad, (Input(UnitMonad)));

let parse = next_token => {
  let stack = Stack.create();
  let add_element = el => {
    let (name, attrs, subels) = Stack.pop(stack);
    Stack.push((name, attrs, [el, ...subels]), stack);
  };

  let rec loop = () =>
    switch (next_token()) {
    | Some(t) =>
      switch (t) {
      | [@implicit_arity] X.StartTag(name, attrs, selfclosing) =>
        let el = (name, attrs, []);
        if (selfclosing) {
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
        };
      | X.EndTag(_name) =>
        if (Stack.length(stack) > 1) {
          let (q, a, els) = Stack.pop(stack);
          add_element(
            [@implicit_arity] Xmlelement(q, List.rev(a), List.rev(els)),
          );
          loop();
        } else {
          loop();
        }
      | X.Text(text) =>
        add_element(Xmlcdata(text));
        loop();
      | X.Doctype(_)
      | X.PI(_) => loop()
      }
    | None => ()
    };

  loop();
  let (q, a, els) = Stack.pop(stack);
  [@implicit_arity] Xmlelement(q, List.rev(a), List.rev(els));
};

let parse_stream = strm => {
  let strm = LS.make_stream(strm);
  let next_token = XmlParser.make_lexer(strm);
  try (parse(next_token)) {
  | [@implicit_arity] S.Located_exn((line, col), exn) =>
    switch (exn) {
    | XmlParser.Exn_msg(msg) =>
      Printf.eprintf("%d:%d %s\n", line, col, msg);
      Pervasives.exit(127);
    | XmlParser.Exn_ExpectedChar(chs) =>
      Printf.eprintf(
        "%d:%d Expected '%s'\n",
        line,
        col,
        String.make(1, List.hd(chs)),
      );
      Pervasives.exit(127);
    | XmlParser.Exn_CharToken(u) =>
      let chs = XmlParser.E.encode_unicode(u);
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

let parse_string = str => {
  let strm = Stream.of_string(str);
  parse_stream(strm);
};

let parse_document = f => {
  let strm = Stream.of_channel(f);
  parse_stream(strm);
};

module Serialization = {
  let string_of_attr = ((name, value)) =>
    name ++ "='" ++ encode(value) ++ "'";

  let string_of_list = (f, sep) =>
    fun
    | [] => ""
    | [x] => f(x)
    | [x, ...xs] =>
      List.fold_left((res, x) => res ++ sep ++ f(x), f(x), xs);

  let rec aux_serialize = out =>
    fun
    | [@implicit_arity] Xmlelement(name, attrs, children) => {
        out("<");
        out(name);
        if (attrs != []) {
          out(" ");
          out(string_of_list(string_of_attr, " ", attrs));
        };
        if (children == []) {
          out("/>");
        } else {
          out(">");
          List.iter(aux_serialize(out), children);
          out("</");
          out(name);
          out(">");
        };
      }
    | Xmlcdata(text) => out(encode(text));

  let serialize_document = (out, xml) => aux_serialize(out, xml);
};
