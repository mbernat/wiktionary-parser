open Dict
exception ExpectedTextFor of string 
exception ExpectedMapFor of string 
exception RequiredFieldMissing of string 
let text k d =
  match d with
  | ((Text (t))[@explicit_arity ]) -> t
  | Map _ -> raise ((ExpectedTextFor (k))[@explicit_arity ])
let map k d =
  match d with
  | Text _ -> raise ((ExpectedMapFor (k))[@explicit_arity ])
  | ((Map (m))[@explicit_arity ]) -> m
let required k l =
  let res = List.find_opt (fun (k', _v) -> k = k') l in
  match res with
  | ((Some (k, v))) -> v
  | None -> raise ((RequiredFieldMissing (k))[@explicit_arity ])
module Option =
  struct
    let map f o =
      match o with
      | ((Some (x))[@explicit_arity ]) -> ((Some ((f x)))[@explicit_arity ])
      | None -> None
  end
let optional_full k l =
  (l |> (List.find_opt (fun (k', v) -> k = k'))) |> (Option.map snd)
let simple k e = (e |> (required k)) |> (text k)
let complex k e = (e |> (required k)) |> (map k)
let optional k e = (e |> (optional_full k)) |> (Option.map (text k))
let optional_with k f e =
  (e |> (optional_full k)) |> (Option.map (fun x -> (x |> (text k)) |> f))
module Contributor =
  struct
    type t = {
      username: string option ;
      id: int option ;
      ip: string option }
    let parse d =
      {
        username = (d |> (optional "username"));
        id = (d |> (optional_with "id" int_of_string));
        ip = (d |> (optional "ip"))
      }
  end
module Revision =
  struct
    type t =
      {
      id: int ;
      parentid: int option ;
      timestamp: string ;
      contributor: Contributor.t ;
      comment: string option ;
      model: string ;
      format: string ;
      text: string }
    let parse d =
      {
        id = ((d |> (simple "id")) |> int_of_string);
        parentid = (d |> (optional_with "parentid" int_of_string));
        timestamp = (d |> (simple "timestamp"));
        contributor = ((d |> (complex "contributor")) |> Contributor.parse);
        comment = (d |> (optional "comment"));
        model = (d |> (simple "model"));
        format = (d |> (simple "format"));
        text = (d |> (simple "text"))
      }
  end
module Page =
  struct
    type t =
      {
      title: string ;
      ns: int ;
      id: int ;
      restrictions: string option ;
      revision: Revision.t }
    let parse d =
      {
        title = (d |> (simple "title"));
        ns = ((d |> (simple "ns")) |> int_of_string);
        id = ((d |> (simple "id")) |> int_of_string);
        restrictions = (d |> (optional "restrictions"));
        revision = ((d |> (complex "revision")) |> Revision.parse)
      }
  end