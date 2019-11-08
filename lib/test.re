let process_page = page_stream => {
  let page = Dump.parse_page_stream(page_stream);
  let text_dict = Text.parse(page.revision.text);
  (page, text_dict)
}

let dump_to_list = filename => {
  open Dump
  let (xml_file, close) = Markup.file(filename);
  let pages = page_stream(xml_file)
    |> Markup.map(process_page)
    |> Markup.to_list
  close();
  pages
}

let test_dump = "dumps/enwiktionary-1e5.xml"

let test_list = dump_to_list(test_dump)

let get_page = page => List.find(((p, _td)) => p.Page.Page.title == page)

let pie = get_page("pie", test_list)