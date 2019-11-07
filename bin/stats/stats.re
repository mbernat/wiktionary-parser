let process_page = page_stream => {
  open Lib
  let page = Dump.parse_page_stream(page_stream);
  let text_dict = Text.parse(page.revision.text);
  let headings = switch(text_dict) {
    | Dict.Text(a) => [a]
    | Dict.Map(m) => List.map(fst, m);
  }
  if (page.title == "pie")
    Dict.print_keys(text_dict);
  String.concat(" ", List.cons(page.title, headings))
}

let process_dump = filename => {
  open Lib.Dump
  let (xml_file, close) = Markup.file(filename);
  let _pages = page_stream(xml_file)
    |> Markup.map(process_page)
    |> Markup.to_list
  close();
  //print_endline(String.concat("\n", pages));
}

let () = {
  switch (Sys.argv) {
    | [|_, filename|] => process_dump(filename)
    | _ => print_endline("Usage: esy stats FILENAME")
  }
}