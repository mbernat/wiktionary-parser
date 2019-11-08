type t =
  | Text of string 
  | Map of (string * t) list 
let print_keys dict =
  let rec go n d =
    match d with
    | ((Text (string))[@explicit_arity ]) -> ()
    | ((Map (m))[@explicit_arity ]) ->
        List.iter
          (fun (k, d') ->
             print_endline (String.concat "" [String.make (2 * n) ' '; k]);
             go (n + 1) d') m in
  go 0 dict