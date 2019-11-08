let test_name name (_ns, name') _attrs = name = name'
let parse_page_stream ps =
  ((((ps |> Dom.get) |> Dom.to_dict) |> (Page.map "root of page")) |>
     (Page.complex "page"))
    |> Page.Page.parse
let page_stream xml_file =
  let open Markup in
    ((((xml_file |> parse_xml) |> signals) |> trim) |> content) |>
      (elements (test_name "page"))