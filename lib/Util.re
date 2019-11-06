/*
TODO parallel processing:
do a fast scan over the whole file and split it into batches to be processed in parallel
*/

let dump_full = "dumps/enwiktionary-latest-pages-articles.xml"
let dump_medium = "dumps/enwiktionary-1e7.xml"
let dump_head = "dumps/enwiktionary-head.xml"
let test_dump = dump_medium

let test_name = name => ((_ns, name'), _attrs) => name == name';

type dom = String(string) | Element(string, list(dom))

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
  // Wiktionary seems to treat nodes with no children as text nodes
  | [] => Page.Text("")
  | [c] => dict_of_dom(c)
  | l => {
    let merged = l
    |> List.map(dom => dom |> dict_of_dom |> Page.map("merging"))
    |> List.flatten;
    Map(merged)
  }
}
and dict_of_dom = dom => switch(dom) {
  | String(s) => Text(s)
  | Element(name, ch) => Map([(name, dict_of_children(ch))])
}

let cut = s => String.sub(s, 2, String.length(s) - 4)

let pairs = l => {
  open Str
  let rec go = (l, acc) => switch(l) {
    | [] => acc
    | [Text(a), ...tl] => go(tl, acc);
    | [Delim(a), Text(b), ...tl] => go(tl, List.cons((cut(a), b), acc))
    | [Delim(a), ...tl] => go(tl, List.cons((cut(a), ""), acc))
  }
  go(l, [])
}

let get_langs = text => {
  open Str
  text
    |> full_split(regexp("^==[^=]+==$"))
    |> pairs
}

let add_page = (ps, page_stream) => {
  open Page
  let page = page_stream
    |> get_dom
    |> from_opt
    |> dict_of_dom
    |> map("root of page")
    |> complex("page")
    |> Page.parse;
  /*
  print_endline(page.title);
  print_endline(page.revision.format);
  */
  let headings = get_langs(page.revision.text) |> List.map(fst);
  let info = String.concat(" ", List.cons(page.title, headings))
  List.cons(info, ps)
}

let hello = () => {
  open Markup
  let (xml_file, close) = file(test_dump);
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