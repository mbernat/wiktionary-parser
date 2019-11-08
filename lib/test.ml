let process_page page_stream =
  let page = Dump.parse_page_stream page_stream in
  let text_dict = Text.parse (page.revision).text in (page, text_dict)
let dump_to_list filename =
  let open Dump in
    let (xml_file, close) = Markup.file filename in
    let pages =
      ((page_stream xml_file) |> (Markup.map process_page)) |> Markup.to_list in
    close (); pages
let test_dump = "dumps/enwiktionary-1e5.xml"
let test_list = dump_to_list test_dump
let get_page page = List.find (fun (p, _td) -> p.Page.Page.title = page)
let pie = get_page "pie" test_list