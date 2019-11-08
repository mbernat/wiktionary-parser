type t =
  | Text of string 
  | Element of string * t list 
exception CouldNotParse 
let from_opt o =
  match o with
  | ((Some (x))) -> x
  | None -> raise CouldNotParse
let get s =
  (s |>
     (Markup.tree
        ~text:(fun ss -> ((Text ((String.concat "" ss)))))
        ~element:(fun (_ns, name) ->
                    fun _attrs ->
                      fun children -> ((Element (name, children))
                        ))))
    |> from_opt
let rec dict_of_children l =
  match l with
  | [] -> ((Dict.Text ("")))
  | c::[] -> to_dict c
  | l ->
      let merged =
        (l |>
           (List.map (fun dom -> (dom |> to_dict) |> (Page.map "merging"))))
          |> List.flatten in
      ((Dict.Map (merged)))
and to_dict dom =
  match dom with
  | ((Text (s))) -> ((Dict.Text (s)))
  | ((Element (name, ch))) ->
      ((Map ([(name, (dict_of_children ch))])))