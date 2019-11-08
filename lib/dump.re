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

let page_stream = xml_file => {
    open Markup
    xml_file
        |> parse_xml
        |> signals
        |> trim
        |> content
        |> elements(test_name("page"))
}