/*
 * (c) 2007-2012 Anastasia Gornostaeva
 *
 * http://www.w3.org/TR/xml (fourth edition)
 * http://www.w3.org/TR/REC-xml-names
 */

exception NonXmlelement;
exception InvalidNS;

type namespace = option(string);

type prefix = string;

type ncname = string;

type name = ncname;

type qname = (namespace, name);

type cdata = string;

type attribute = (qname, cdata);

type element =
  | Xmlelement((qname, list(attribute), list(element)))
  | Xmlcdata(cdata);

let ns_xml = Some("http://www.w3.org/XML/1998/namespace");

let no_ns = None;

let encode = Xml_encode.encode;
let decode = Xml_decode.decode;

module Serialization = {
  type t = {
    mutable tmp_prefix: int,
    default_nss: list(namespace),
    bindings: Hashtbl.t(string, string),
  };

  let get_default_nss = t => t.default_nss;

  let bind_prefix = (t, prefix, namespace) =>
    switch (namespace) {
    | None => raise(InvalidNS)
    | Some(str) => Hashtbl.add(t.bindings, str, prefix)
    };

  let create = default_nss => {
    let bindings = Hashtbl.create(5);
    let t = {tmp_prefix: 0, default_nss, bindings};
    bind_prefix(t, "xml", ns_xml);
    t;
  };

  let string_of_qname = (t, (ns, name)) => {
    let prefix =
      switch (ns) {
      | None => ""
      | Some(str) =>
        try (Hashtbl.find(t.bindings, str)) {
        | Not_found => ""
        }
      };

    if (prefix == "") {
      name;
    } else {
      prefix ++ ":" ++ name;
    };
  };

  let string_of_attr = (t, (qname, value)) =>
    string_of_qname(t, qname) ++ "='" ++ encode(value) ++ "'";

  let string_of_list = (f, sep) =>
    fun
    | [] => ""
    | [x] => f(x)
    | [x, ...xs] =>
      List.fold_left((res, x) => res ++ sep ++ f(x), f(x), xs);

  let local_namespaces = (lnss, t, (ns, _name), attrs) => {
    let lnss =
      if (List.mem(ns, lnss) || List.mem(ns, t.default_nss)) {
        lnss;
      } else {
        [ns, ...lnss];
      };

    List.fold_left(
      (acc, ((ns, _name), _value)) =>
        if (ns == no_ns
            || ns == ns_xml
            || List.mem(ns, t.default_nss)
            || List.mem(ns, lnss)) {
          acc;
        } else {
          switch (ns) {
          | None => acc
          | Some(str) =>
            if (!Hashtbl.mem(t.bindings, str)) {
              t.tmp_prefix = t.tmp_prefix + 1;
              let p = "ns" ++ string_of_int(t.tmp_prefix);
              bind_prefix(t, p, ns);
            };
            [ns, ...acc];
          };
        },
      lnss,
      attrs,
    );
  };

  let string_of_ns = t =>
    fun
    | None => "xmlns=''"
    | Some(str) => {
        let prefix =
          try (Hashtbl.find(t.bindings, str)) {
          | Not_found => ""
          };

        if (prefix == "") {
          "xmlns='" ++ encode(str) ++ "'";
        } else {
          "xmlns:" ++ prefix ++ "='" ++ encode(str) ++ "'";
        };
      };

  let rec aux_serialize = (lnss, t, out) =>
    fun
    | [@implicit_arity] Xmlelement(qname, attrs, children) => {
        let lnss = local_namespaces(lnss, t, qname, attrs);
        out("<");
        out(string_of_qname(t, qname));
        if (attrs != []) {
          out(" ");
          out(string_of_list(string_of_attr(t), " ", attrs));
        };
        if (lnss != []) {
          out(" ");
          out(string_of_list(string_of_ns(t), " ", lnss));
        };
        if (children == []) {
          out("/>");
        } else {
          out(">");
          List.iter(
            aux_serialize(
              [],
              {...t, default_nss: lnss @ t.default_nss},
              out,
            ),
            children,
          );
          out("</");
          out(string_of_qname(t, qname));
          out(">");
        };
      }
    | Xmlcdata(text) => out(encode(text));

  let serialize_document = (t, out, xml) =>
    aux_serialize(t.default_nss, t, out, xml);
};

let get_qname =
  fun
  | [@implicit_arity] Xmlelement(qname, _, _) => qname
  | Xmlcdata(_) => raise(NonXmlelement);

let get_namespace = ((namespace, _name)) => namespace;

let get_name = ((_namespace, name)) => name;

let get_attrs = (~ns=?) =>
  fun
  | [@implicit_arity] Xmlelement(_', attrs, _) =>
    switch (ns) {
    | None => attrs
    | Some(v) => List.find_all((((ns', _), _)) => ns' == v, attrs)
    }
  | Xmlcdata(_) => raise(NonXmlelement);

let get_attr_value = (~ns=?, name, attrs) => {
  let (_, value) =
    List.find(
      ((qname, _)) =>
        switch (ns) {
        | None => (no_ns, name) == qname
        | Some(v) => (v, name) == qname
        },
      attrs,
    );

  value;
};

let safe_get_attr_value = (~ns=?, name, attrs) =>
  try (get_attr_value(~ns?, name, attrs)) {
  | Not_found => ""
  };

let get_element = (qname, childs) =>
  List.find(
    fun
    | [@implicit_arity] Xmlelement(qname', _, _) => qname == qname'
    | Xmlcdata(_) => false,
    childs,
  );

let get_elements = (qname, childs) =>
  List.filter(
    fun
    | [@implicit_arity] Xmlelement(qname', _, _) => qname == qname'
    | Xmlcdata(_) => false,
    childs,
  );

let get_children =
  fun
  | [@implicit_arity] Xmlelement(_, _, children) => children
  | Xmlcdata(_) => raise(NonXmlelement);

let get_subelement = (qname, el) => get_element(qname, get_children(el));

let get_subelements = (qname, el) => get_elements(qname, get_children(el));

let get_first_element = els =>
  List.find(
    fun
    | Xmlelement(_) => true
    | Xmlcdata(_) => false,
    els,
  );

let collect_cdata = els => {
  let res =
    List.fold_left(
      acc =>
        fun
        | Xmlcdata(cdata) => [cdata, ...acc]
        | Xmlelement(_) => acc,
      [],
      els,
    );
  String.concat("", List.rev(res));
};

let get_cdata = el => collect_cdata(get_children(el));

let remove_cdata = els =>
  List.filter(
    fun
    | Xmlelement(_) => true
    | Xmlcdata(_) => false,
    els,
  );

let make_element = (qname, attrs, children) =>
  [@implicit_arity] Xmlelement(qname, attrs, children);

let make_attr = (~ns=?, name, value) => {
  let ns =
    switch (ns) {
    | None => no_ns
    | Some(v) => v
    };
  ((ns, name), value);
};

let make_simple_cdata = (qname, cdata) =>
  [@implicit_arity] Xmlelement(qname, [], [Xmlcdata(cdata)]);

let mem_qname = (qname, els) =>
  List.exists(
    fun
    | [@implicit_arity] Xmlelement(qname', _, _) => qname == qname'
    | Xmlcdata(_) => false,
    els,
  );

let mem_child = (qname, el) => mem_qname(qname, get_children(el));

let iter = (f, el) => List.iter(f, get_children(el));

/*
 * Parsing
 */

let split_name = name =>
  if (String.contains(name, ':')) {
    let idx = String.index(name, ':');
    let prefix = String.sub(name, 0, idx);
    let lname =
      if (idx + 1 > String.length(name)) {
        "";
      } else {
        String.sub(name, idx + 1, String.length(name) - (idx + 1));
      };

    (prefix, lname);
  } else {
    ("", name);
  };

let split_attrs = attrs =>
  List.fold_left(
    ((nss, attrs), (name, value)) => {
      let (prefix, lname) = split_name(name);
      if (prefix == "" && lname == "xmlns") {
        let ns =
          if (value == "") {
            None;
          } else {
            Some(value);
          };
        ([(ns, ""), ...nss], attrs);
      } else if (prefix == "xmlns" && lname != "") {
        let ns =
          if (value == "") {
            None;
          } else {
            Some(value);
          };
        ([(ns, lname), ...nss], attrs);
      } else {
        (nss, [((prefix, lname), value), ...attrs]);
      };
    },
    ([], []),
    attrs,
  );

let add_namespaces = (namespaces, nss) =>
  List.iter(((ns, prefix)) => Hashtbl.add(namespaces, prefix, ns), nss);

let remove_namespaces = (namespaces, nss) =>
  List.iter(((_ns, prefix)) => Hashtbl.remove(namespaces, prefix), nss);

let parse_qname = (nss, (prefix, lname)) =>
  try (
    {
      let namespace = Hashtbl.find(nss, prefix);
      (namespace, lname);
    }
  ) {
  | Not_found => (no_ns, lname)
  };

let parse_qname_attribute = (nss, (prefix, lname)) =>
  if (prefix == "") {
    (no_ns, lname);
  } else {
    try (
      {
        let ns = Hashtbl.find(nss, prefix);
        (ns, lname);
      }
    ) {
    | Not_found => (no_ns, lname)
    };
  };

let parse_attrs = (nss, attrs) =>
  List.map(
    ((name, value)) => (parse_qname_attribute(nss, name), value),
    attrs,
  );

let parse_element_head = (namespaces, name, attrs) => {
  let (lnss, attrs) = split_attrs(attrs);
  add_namespaces(namespaces, lnss);
  let qname = parse_qname(namespaces, split_name(name));
  let attrs = parse_attrs(namespaces, attrs);
  (qname, lnss, attrs);
};

let string_of_tag = ((ns, name)) => {
  let prefix =
    switch (ns) {
    | None => ""
    | Some(str) => "URI " ++ str
    };

  Printf.sprintf("(%S) %s", prefix, name);
};
