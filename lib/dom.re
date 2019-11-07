type t = Text(string) | Element(string, list(t))

exception CouldNotParse

let from_opt = o => switch(o) {
  | Some(x) => x
  | None => raise(CouldNotParse)
}

let get = s => s |> Markup.tree(
    ~text=(ss => Text(String.concat("", ss))),
    ~element=(((_ns, name), _attrs, children) => Element(name, children))
  ) |> from_opt;


let rec dict_of_children = l => switch(l) {
  // Wiktionary seems to treat nodes with no children as text nodes
  | [] => Page.Text("")
  | [c] => to_dict(c)
  | l => {
    let merged = l
    |> List.map(dom => dom |> to_dict |> Page.map("merging"))
    |> List.flatten;
    Page.Map(merged)
  }
}
and to_dict = dom => switch(dom) {
  | Text(s) => Page.Text(s)
  | Element(name, ch) => Map([(name, dict_of_children(ch))])
}