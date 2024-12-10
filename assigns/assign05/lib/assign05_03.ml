type ident = string

type ty = 
| Unit
| Arr of ty * ty

type expr = 
| Var of ident
| Fun of ident * ty * expr
| App of expr * expr

type ctxt = (ident * ty) list


let get_value o = 
  match o with
  | Some v -> v
  | None -> failwith "Expected a value"


let rec type_of ctxt expr = 
  match expr with
  | Var i -> Some (List.assoc i ctxt)
  | Fun (i, t, e) -> Some (Arr (t, get_value(type_of ((i, t) :: ctxt) e)))
  | App (e1, e2) ->
      match type_of ctxt e1 with
      | Some (Arr (t1, t2)) when type_of ctxt e2 = Some t1 -> Some t2
      | _ -> None