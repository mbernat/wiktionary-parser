type t = Text(string) | Map(list((string, t)))

let print_keys = dict => {
    let rec go = (n, d) => switch(d) {
        | Text(string) => ()
        | Map(m) => List.iter(((k, d')) => {
            print_endline(String.concat("", [String.make(2*n, ' '), k]));
            go(n+1, d')
        }, m)
    }
    go(0, dict)
}