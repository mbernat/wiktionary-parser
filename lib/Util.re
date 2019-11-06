//open Markup

/*
TODO parallel processing:
do a fast scan over the whole file and split it into batches to be processed in paralles
*/

/*
parsing optional elements:
A
option(B)
C

AC -> A ..ok.. B ..nope .. C .. ok ..
ABC

the list of parsers is full
the list of elements may contain holes
so fold on parser list:
if required: parse or error
if optional: parse -> some, no parse -> none & move onto the next parser
*/

let dump_full = "dumps/enwiktionary-latest-pages-articles.xml"
let dump_medium = "dumps/enwiktionary-1e5.xml"
let dump_head = "dumps/enwiktionary-head.xml"

let test_name = name => ((_ns, name'), _attrs) => name == name';

type dom = | Text(string) | Element(string, list(dom))

type parse_error =
  | ExpectedTextGotElement(string)
  | ExpectedElementGotText(string, string)
  | ExpectedElementGotElement(string, string)

type parser('a) = dom => result('a, parse_error)
type field('a) = Required(parser('a)) | Optional(parser('a))

let rec text = p => switch(p) {
  | Text(t) => Ok(t)
  | Element(name, _cs) => Error(ExpectedTextGotElement(name))
}

/*
TODO figure out how do data-driven xml parsing
*/

// parse_record: [parser], [dom] -> [parsed things]
// f

let rec parse_record = (parsers, cs) => switch(parsers) {
  | [] => []
  | [p, ...tl] => []
}

let parse_element = (name, f, e) => {print_endline(name); switch(e) {
  | Text(t) => Error(ExpectedElementGotText(name, t))
  | Element(name', cs) =>
    if (name == name')
      f(cs)
    else
      Error(ExpectedElementGotElement(name, name'))
}}

let bind = (r, f) => switch(r) {
  | Ok(a) => f(a)
  | Error(b) => Error(b)
}

let single = (name, e) => parse_element(name, e' => e' |> List.hd |> text, e)

module Contributor {
  type t = {
    username: string,
    id: int
  }

  let parsers = [
    Required(single("username")),
    Required(single("id"))
  ];

  let parse_contributor = cs => {
    let [username, id] = parse_record(parsers, cs);
    Ok({
      username: username,
      id: id |> int_of_string
    })
  }

  let parse = parse_element("contributor", parse_contributor)
}

module Revision {
  type t = {
    id: int,
    parentid: int,
    timestamp: string,
    contributor: Contributor.t,
    comment: string,
    model: string,
    format: string,
    text: string
  }

  let parsers = [
    Required(single("id")),
    Required(single("parentid")),
    Required(single("timestamp")),
    Required(Contributor.parse),
    Required(single("comment")),
    Required(single("model")),
    Required(single("format")),
    Required(single("text"))
  ];

  let parse_revision = cs => {
    let [id, parentid, timestamp, contributor, comment, model, format, text] = parse_record(parsers, cs);
    Ok({
      id: id |> int_of_string,
      parentid: parentid |> int_of_string,
      timestamp: timestamp,
      contributor: contributor,
      comment: comment,
      model: model,
      format: format,
      text: text
    })
}

  let parse = parse_element("revision", parse_revision)
}

module Page {
  type t = {
    title: string,
    ns: int,
    id: int,
    restrictions: option(string),
    revision: Revision.t
  }

  let parsers = [
    Required(text("title")),
    Required(text("ns")),
    Required(text("id")),
    Optional(text("restrictions")),
    Required(Revision.parse)
  ]

  let parse_page = cs => {
    let [title, ns, id, restrictions, revision] = parse_record(parsers, cs);
    Ok({
      title: title,
      ns: ns |> int_of_string,
      id: id |> int_of_string,
      restrictions: restrictions,
      revision: revision
    })
  }

  let parse = parse_element("page", parse_page)
}

let get_dom = s => s |> Markup.tree(
    ~text=(ss => Text(String.concat("", ss))),
    ~element=(((_ns, name), _attrs, children) => Element(name, children))
  );

exception FoundNone

let from_opt = o => switch(o) {
  | Some(x) => x
  | None => raise(FoundNone)
}

let string_of = p => p |> Markup.write_xml |> Markup.to_string;


let add_page = (ps, page_stream) => {
  print_endline("processing page")
  //print_endline(string_of(page_stream))
  open Page
  let page = page_stream |> get_dom |> from_opt |> parse;
  List.cons(page.title, ps)
}

let hello = () => {
  open Markup
  let (xml_file, close) = file(dump_medium);
  let pages = xml_file
  |> parse_xml
  |> signals
  |> content
  |> elements(test_name("page"))
  |> fold(add_page, [])
  close();
  let info = String.concat("\n", pages);
  let _count = string_of_int(List.length(pages));
  info
}