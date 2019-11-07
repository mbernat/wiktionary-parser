// TODO account for multiple etymologies and word types (noun, verb, ...)
// There can be several etymologies and each can be of several types
/*
etymology is either default or numbered;
if it's default or missing, the root section pertains a single etymology
if it's numbered, the root section lists shared properties that are used by all meanings
*/

type kind = Noun(string) | Verb(string) | Other(string)

type etymology = string;

type meaning = {
    kind: kind,
    etymology: option(etymology),
    pronunciation: string
}

type word = {
    meanings: list(meaning)
}

type language = string
type text = list((language, word))

let cut = (n, s) => String.sub(s, n, String.length(s) - 2*n)

let heading = n => {
    let chain = String.make(n, '=');
    String.concat("", ["^", chain, "[^=]+", chain, "$"])
      |> Str.regexp
}

let rec parse_headings = (n, text) => {
  open Str
  let rec go = (l, acc) => switch(l) {
    | [] => acc
    | [Text(a), ...tl] => go(tl, acc);
    | [Delim(a), Text(b), ...tl] => go(tl, List.cons((cut(n, a), parse_headings(n+1, b)), acc))
    | [Delim(a), ...tl] => go(tl, List.cons((cut(n, a), Dict.Text("")), acc))
  }
  let splits = Str.full_split(heading(n), text);
  switch (splits) {
    | [Text(a)] => Dict.Text(a)
    | l => Dict.Map(go(l, []))
  }
}

let parse = parse_headings(2)