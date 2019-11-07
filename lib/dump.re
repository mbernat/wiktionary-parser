/*
TODO parallel processing:
do a fast scan over the whole file and split it into batches to be processed in parallel
*/

let test_name = name => ((_ns, name'), _attrs) => name == name';

let parse_page_stream = ps =>
  ps
    |> Dom.get
    |> Dom.to_dict
    |> Page.map("root of page")
    |> Page.complex("page")
    |> Page.Page.parse;

let add_page = (ps, page_stream) => {
  open Page
  let page = parse_page_stream(page_stream);
  let headings = Text.get_langs(page.revision.text) |> List.map(fst);
  let info = String.concat(" ", List.cons(page.title, headings))
  List.cons(info, ps)
}

let process = (filename, f) => {
  open Markup
  let (xml_file, close) = file(filename);
  let pages = xml_file
  |> parse_xml
  |> signals
  |> trim
  |> content
  |> elements(test_name("page"))
  |> fold(f, []);
  (pages, close)
}