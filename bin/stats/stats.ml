let process_page page_stream =
  let open Lib in
    let page = Dump.parse_page_stream page_stream in
    let text_dict = Text.parse (page.revision).text in
    let headings =
      match text_dict with
      | ((Dict.Text (a))[@explicit_arity ]) -> [a]
      | ((Dict.Map (m))[@explicit_arity ]) -> List.map fst m in
    String.concat " " (List.cons page.title headings)
let process_dump filename =
  let open Lib.Dump in
    let (xml_file, close) = Markup.file filename in
    let () =
      ((page_stream xml_file) |> (Markup.map process_page)) |>
        (Markup.iter print_endline) in
    close ()
let () =
  match Sys.argv with
  | [|_;filename|] -> process_dump filename
  | _ -> print_endline "Usage: esy stats FILENAME"