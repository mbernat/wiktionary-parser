let cut = s => String.sub(s, 2, String.length(s) - 4)

let pairs = l => {
  open Str
  let rec go = (l, acc) => switch(l) {
    | [] => acc
    | [Text(a), ...tl] => go(tl, acc);
    | [Delim(a), Text(b), ...tl] => go(tl, List.cons((cut(a), b), acc))
    | [Delim(a), ...tl] => go(tl, List.cons((cut(a), ""), acc))
  }
  go(l, [])
}

let get_langs = text => {
  open Str
  text
    |> full_split(regexp("^==[^=]+==$"))
    |> pairs
}