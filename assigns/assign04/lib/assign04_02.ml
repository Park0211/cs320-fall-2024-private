type expr = 
  | True
  | False
  | Num of int
  | Or of expr * expr
  | Add of expr * expr
  | IfThenElse of expr * expr * expr

type ty = 
  | Int
  | Bool

let rec type_of expr =
  match expr with
  | True -> Some Bool
  | False -> Some Bool
  | Num _ -> Some Int
  | Or (e1, e2) -> 
      if type_of e1 = Some Bool && type_of e2 = Some Bool then
        Some Bool
      else
        None
  | Add (e1, e2) -> 
      if type_of e1 = Some Int && type_of e2 = Some Int then
        Some Int
      else
        None
  | IfThenElse (e1, e2, e3) ->
      let type_of_e2 = type_of e2 in
      if type_of e1 = Some Bool && type_of_e2 = type_of e3 then
        type_of_e2
      else
        None