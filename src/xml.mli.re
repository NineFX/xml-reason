/*
 * (c) 2007-2012 Anastasia Gornostaeva
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

let ns_xml: namespace;
let no_ns: namespace;

let encode: string => string;
let decode: string => string;

module Serialization: {
  type t = {
    mutable tmp_prefix: int,
    default_nss: list(namespace),
    bindings: Hashtbl.t(string, string),
  };
  let get_default_nss: t => list(namespace);
  let bind_prefix: (t, string, namespace) => unit;
  let create: list(namespace) => t;
  let string_of_qname: (t, qname) => string;
  let string_of_attr: (t, attribute) => string;
  let string_of_list: ('a => string, string, list('a)) => string;
  let local_namespaces:
    (list(namespace), t, qname, list(attribute)) => list(namespace);
  let string_of_ns: (t, namespace) => string;
  let aux_serialize: (list(namespace), t, string => unit, element) => unit;
  let serialize_document: (t, string => unit, element) => unit;
};

let get_qname: element => qname;
let get_namespace: qname => namespace;
let get_name: qname => string;
let get_attrs: (~ns: namespace=?, element) => list(attribute);
let get_attr_value: (~ns: namespace=?, name, list(attribute)) => cdata;
let safe_get_attr_value: (~ns: namespace=?, name, list(attribute)) => string;
let get_element: (qname, list(element)) => element;
let get_elements: (qname, list(element)) => list(element);
let get_children: element => list(element);
let get_subelement: (qname, element) => element;
let get_subelements: (qname, element) => list(element);
let get_first_element: list(element) => element;
let collect_cdata: list(element) => string;
let get_cdata: element => string;
let remove_cdata: list(element) => list(element);
let make_element: (qname, list(attribute), list(element)) => element;
let make_attr: (~ns: namespace=?, name, cdata) => attribute;
let make_simple_cdata: (qname, cdata) => element;
let mem_qname: (qname, list(element)) => bool;
let mem_child: (qname, element) => bool;
let iter: (element => unit, element) => unit;

let split_name: string => (prefix, ncname);

let split_attrs:
  list((string, string)) =>
  (list((namespace, string)), list(((string, string), string)));
let add_namespaces:
  (Hashtbl.t(prefix, namespace), list((namespace, prefix))) => unit;
let remove_namespaces:
  (Hashtbl.t(prefix, namespace), list((namespace, prefix))) => unit;
let parse_qname: (Hashtbl.t(prefix, namespace), (prefix, string)) => qname;
let parse_qname_attribute:
  (Hashtbl.t(prefix, namespace), (prefix, string)) => qname;
let parse_attrs:
  (Hashtbl.t(prefix, namespace), list(((prefix, string), string))) =>
  list((qname, string));
let parse_element_head:
  (Hashtbl.t(prefix, namespace), string, list((string, string))) =>
  (qname, list((namespace, prefix)), list(attribute));
let string_of_tag: qname => string;
