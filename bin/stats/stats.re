let dump_full = "dumps/enwiktionary-latest-pages-articles.xml"
let dump_medium = "dumps/enwiktionary-1e5.xml"
let dump_head = "dumps/enwiktionary-head.xml"
let test_dump = dump_medium

let process_dump = () => {
  open Lib.Dump
  let (pages, close) = process(test_dump, add_page)
  close();
  String.concat("\n", pages);
}

let () = print_endline(process_dump())