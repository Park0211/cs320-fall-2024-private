open Utils

let rec type_of (e: expr) : ty option=
  match e with
  | Num _-> Some TInt
  | Add _-> Some TInt
  | Lt _-> Some TBool
  | Ite (_, e1, _) -> type_of e1