let process_page = page_stream => {
  open Lib
  let page = Dump.parse_page_stream(page_stream);
  let headings = Text.get_langs(page.revision.text) |> List.map(fst);
  String.concat(" ", List.cons(page.title, headings))
}

let process_dump = () => {
  open Lib.Dump
  let filename = Sys.argv[1];
  let (xml_file, close) = Markup.file(filename);
  let pages = page_stream(xml_file)
    |> Markup.map(process_page)
    |> Markup.to_list
  close();
  String.concat("\n", pages);
}

let () = print_endline(process_dump())