type ident = string

type expr' = 
  | True
  | False
  | Num of int
  | Var of ident
  | Let of ident * expr' * expr'
  | Add of expr' * expr'
  | Or of expr' * expr'
  | IfThenElse of expr' * expr' * expr'

type ty' = 
  | Int
  | Bool

type context = (ident * ty') list

let option_get x =
  match x with
  | Some v -> v
  | None -> failwith "Expected a value"

let rec assoc k l =
  match l with
  | [] -> None
  | (k', v) :: l' -> if k = k' then Some v else assoc k l'

let rec type_of' ctx expr = 
  match expr with
  | True -> Some Bool
  | False -> Some Bool
  | Num _ -> Some Int
  | Var i -> assoc i ctx
  | Let (i, e1, e2) -> 
      let ty1 = type_of' ctx e1 in
      let ctx' = (i, option_get ty1) :: ctx in
      type_of' ctx' e2
  | Add (e1, e2) ->
      if type_of' ctx e1 = Some Int && type_of' ctx e2 = Some Int then
        Some Int
      else
        None
  | Or (e1, e2) ->
      if type_of' ctx e1 = Some Bool && type_of' ctx e2 = Some Bool then
        Some Bool
      else
        None
  | IfThenElse (e1, e2, e3) ->
      if type_of' ctx e1 = Some Bool && type_of' ctx e2 = type_of' ctx e3 then
        type_of' ctx e2
      else
        None