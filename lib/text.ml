type kind =
  | Noun of string 
  | Verb of string 
  | Other of string 
type etymology = string
type meaning =
  {
  kind: kind ;
  etymology: etymology option ;
  pronunciation: string }
type word = {
  meanings: meaning list }
type language = string
type text = (language * word) list
let cut n s = String.sub s n ((String.length s) - (2 * n))
let heading n =
  let chain = String.make n '=' in
  (String.concat "" ["^"; chain; "[^=]+"; chain; "$"]) |> Str.regexp
let rec parse_headings n text =
  let open Str in
    let rec go l acc =
      match l with
      | [] -> acc
      | ((Text (a))[@explicit_arity ])::tl -> go tl acc
      | ((Delim (a))[@explicit_arity ])::((Text (b))[@explicit_arity ])::tl
          -> go tl (List.cons ((cut n a), (parse_headings (n + 1) b)) acc)
      | ((Delim (a))[@explicit_arity ])::tl ->
          go tl
            (List.cons ((cut n a), ((Dict.Text (""))[@explicit_arity ])) acc) in
    let splits = Str.full_split (heading n) text in
    match splits with
    | ((Text (a))[@explicit_arity ])::[] -> ((Dict.Text (a))
        [@explicit_arity ])
    | l -> ((Dict.Map ((go l [])))[@explicit_arity ])
let parse = parse_headings 2