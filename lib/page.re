open Dict

exception ExpectedTextFor(string)
exception ExpectedMapFor(string)
exception RequiredFieldMissing(string)

let text = (k, d) => switch(d) {
    | Text(t) => t
    | Map(_) => raise(ExpectedTextFor(k))
}

let map = (k, d) => switch(d) {
    | Text(_) => raise(ExpectedMapFor(k))
    | Map(m) => m
}

let required = (k, l) => {
    let res = List.find_opt(((k', _v)) => k == k', l);
    switch (res) {
        | Some((k, v)) => v
        | None => raise(RequiredFieldMissing(k))
    }
}

module Option {
    let map = (f, o) => switch(o) {
        | Some(x) => Some(f(x))
        | None => None
    }
}

let optional_full = (k, l) => {
    l
    |> List.find_opt(((k', v)) => k == k')
    |> Option.map(snd)
}

let simple = (k, e) => e |> required(k) |> text(k);
let complex = (k, e) => e |> required(k) |> map(k);
let optional = (k, e) => e |> optional_full(k) |> Option.map(text(k))
let optional_with = (k, f, e) => e |> optional_full(k) |> Option.map(x => x |> text(k) |> f)

module Contributor {
    type t = {
        username: option(string),
        id: option(int),
        ip: option(string)
    }

    let parse = d => {
        username: d |> optional("username"),
        id: d |> optional_with("id", int_of_string),
        ip: d |> optional("ip")
    }
}

module Revision {
    type t = {
        id: int,
        parentid: option(int),
        timestamp: string,
        contributor: Contributor.t,
        comment: option(string),
        model: string,
        format: string,
        text: string
    }

    let parse = d => {
        id: d |> simple("id") |> int_of_string,
        parentid: d |> optional_with("parentid", int_of_string),
        timestamp: d |> simple("timestamp"),
        contributor: d |> complex("contributor") |> Contributor.parse,
        comment: d |> optional("comment"),
        model: d |> simple("model"),
        format: d |> simple("format"),
        text: d |> simple("text")
    }
}

module Page {
    type t = {
        title: string,
        ns: int,
        id: int,
        restrictions: option(string),
        revision: Revision.t
    }

    let parse = d => {
        title: d |> simple("title"),
        ns: d |> simple("ns") |> int_of_string,
        id: d |> simple("id") |> int_of_string,
        restrictions: d |> optional("restrictions"),
        revision: d |> complex("revision") |> Revision.parse
    }
}