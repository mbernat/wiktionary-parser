/*
TODO parallel processing:
do a fast scan over the whole file and split it into batches to be processed in parallel
*/

let dump_full = "dumps/enwiktionary-latest-pages-articles.xml"
let dump_medium = "dumps/enwiktionary-1e6.xml"
let dump_head = "dumps/enwiktionary-head.xml"

let test_name = name => ((_ns, name'), _attrs) => name == name';

type dom = String(string) | Element(string, list(dom))
type dict = Text(string) | Map(list((string, dict)))

exception ExpectedTextFor(string)
exception ExpectedMapFor(string)
exception RequiredFieldMissing(string)

let text = (k, d) => switch(d) {
  | Text(t) => t
  | Map(_) => raise(ExpectedTextFor(k))
}

let map = (k, d) => switch(d) {
  | Text(_) => raise(ExpectedMapFor(k))
  | Map(m) => m
}

let required = (k, l) => {
  let res = List.find_opt(((k', _v)) => k == k', l);
  switch (res) {
    | Some((k, v)) => v
    | None => raise(RequiredFieldMissing(k))
  }
}

let optional_full = (k, l) => {
  let res = List.find_opt(((k', v)) => k == k', l);
  switch (res) {
    | Some((k, v)) => Some(v)
    | None => None
  }
}

module Option {
  let map = (f, o) => switch(o) {
    | Some(x) => Some(f(x))
    | None => None
  }
}

let simple = (k, e) => e |> required(k) |> text(k);
let complex = (k, e) => e |> required(k) |> map(k);
let optional = (k, e) => e |> optional_full(k) |> Option.map(text(k))
let optional_with = (k, f, e) => e |> optional_full(k) |> Option.map(x => x |> text(k) |> f)


module Contributor {
  type t = {
    username: option(string),
    id: option(int),
    ip: option(string)
  }

  let parse = d => {
    username: d |> optional("username"),
    id: d |> optional_with("id", int_of_string),
    ip: d |> optional("ip")
  }
}

module Revision {
  type t = {
    id: int,
    parentid: option(int),
    timestamp: string,
    contributor: Contributor.t,
    comment: option(string),
    model: string,
    format: string,
    text: string
  }

  let parse = d => {
    id: d |> simple("id") |> int_of_string,
    parentid: d |> optional_with("parentid", int_of_string),
    timestamp: d |> simple("timestamp"),
    contributor: d |> complex("contributor") |> Contributor.parse,
    comment: d |> optional("comment"),
    model: d |> simple("model"),
    format: d |> simple("format"),
    text: d |> simple("text")
  }
}

module Page {
  type t = {
    title: string,
    ns: int,
    id: int,
    restrictions: option(string),
    revision: Revision.t
  }

  let parse = d => {
    title: d |> simple("title"),
    ns: d |> simple("ns") |> int_of_string,
    id: d |> simple("id") |> int_of_string,
    restrictions: d |> optional("restrictions"),
    revision: d |> complex("revision") |> Revision.parse
  }
}

let get_dom = s => s |> Markup.tree(
    ~text=(ss => String(String.concat("", ss))),
    ~element=(((_ns, name), _attrs, children) => Element(name, children))
  );

exception FoundNone

let from_opt = o => switch(o) {
  | Some(x) => x
  | None => raise(FoundNone)
}

let string_of = p => p |> Markup.write_xml |> Markup.to_string;

let rec dict_of_children = l => switch(l) {
  | [c] => dict_of_dom(c)
  | l => {
    let merged = l
    |> List.map(dom => dom |> dict_of_dom |> map("merging"))
    |> List.flatten;
    Map(merged)
  }
}
and dict_of_dom = dom => switch(dom) {
  | String(s) => Text(s)
  | Element(name, ch) => Map([(name, dict_of_children(ch))])
}

let add_page = (ps, page_stream) => {
  open Page
  let page = page_stream
    |> get_dom
    |> from_opt
    |> dict_of_dom
    |> map("root of page")
    |> complex("page")
    |> parse;
  List.cons(page.title, ps)
}

let hello = () => {
  open Markup
  let (xml_file, close) = file(dump_medium);
  let pages = xml_file
  |> parse_xml
  |> signals
  |> trim
  |> content
  |> elements(test_name("page"))
  |> fold(add_page, [])
  close();
  let info = String.concat("\n", pages);
  let _count = string_of_int(List.length(pages));
  info
}