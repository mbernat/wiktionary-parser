type t =
  | Text of string 
  | Element of string * t list 
exception CouldNotParse 
let from_opt o =
  match o with
  | ((Some (x))[@explicit_arity ]) -> x
  | None -> raise CouldNotParse
let get s =
  (s |>
     (Markup.tree
        ~text:(fun ss -> ((Text ((String.concat "" ss)))[@explicit_arity ]))
        ~element:(fun (_ns, name) ->
                    fun _attrs ->
                      fun children -> ((Element (name, children))
                        [@explicit_arity ]))))
    |> from_opt
let rec dict_of_children l =
  match l with
  | [] -> ((Dict.Text (""))[@explicit_arity ])
  | c::[] -> to_dict c
  | l ->
      let merged =
        (l |>
           (List.map (fun dom -> (dom |> to_dict) |> (Page.map "merging"))))
          |> List.flatten in
      ((Dict.Map (merged))[@explicit_arity ])
and to_dict dom =
  match dom with
  | ((Text (s))[@explicit_arity ]) -> ((Dict.Text (s))[@explicit_arity ])
  | ((Element (name, ch))[@explicit_arity ]) ->
      ((Map ([(name, (dict_of_children ch))]))[@explicit_arity ])