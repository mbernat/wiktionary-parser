open Markup

let dump_full = "dumps/enwiktionary-latest-pages-articles.xml"
let dump_medium = "dumps/enwiktionary-1e6.xml"
let dump_head = "dumps/enwiktionary-head.xml"

let test_name = name => ((_ns, name'), _attrs) => name == name';

type dom = | Text(string) | Element(name, list(dom))

let rec get_title = p => switch(p) {
  | Text(t) => t
  | Element((_ns, name), c) => get_title(List.hd(c))
}

let get_name_or_text = p => switch(p) {
  | Text(t) => t
  | Element((_ns, name), _c) => name
}

let get_dom = s => s |> tree(
    ~text=(ss => Text(String.concat("", ss))),
    ~element=((name, _attrs, children) => Element(name, children))
  );

let string_of = p => p |> write_xml |> to_string;

let add_page = (ps, s) => {
  let titles = s
    |> elements(test_name("title"))
    |> fold((l, t) => List.cons(string_of(t), l), []);
  List.cons(String.concat("", titles), ps)
}

let hello = () => {
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