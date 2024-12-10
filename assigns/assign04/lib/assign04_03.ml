open Assign04_02

type value = 
| VNum of int
| VBool of bool

let get_bool x =
  match x with
  | VBool b -> b
  | _ -> failwith "Expected a boolean"

let get_int x =
  match x with
  | VNum n -> n
  | _ -> failwith "Expected an integer"

let rec eval expr =
  match expr with
  | True -> VBool true
  | False -> VBool false
  | Num n -> VNum n
  | Or (e1, e2) -> 
      VBool (get_bool (eval e1) || get_bool (eval e2))
  | Add (e1, e2) -> 
      VNum (get_int (eval e1) + get_int (eval e2))
  | IfThenElse (e1, e2, e3) ->
      if get_bool (eval e1) then
        eval e2
      else
        eval e3