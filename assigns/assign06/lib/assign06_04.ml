open Utils

let rec eval (e: expr) : value =
  match e with
  | Num n -> VNum n
  | Add (e1, e2) -> (
    match eval e1, eval e2 with
    | VNum n1, VNum n2 -> VNum (n1 + n2)
    | _ -> failwith "Type error"
  )
  | Lt (e1, e2) -> (
    match eval e1, eval e2 with
    | VNum n1, VNum n2 -> VBool (n1 < n2)
    | _ -> failwith "Type error"
  )
  | Ite (cond, e1, e2) -> (
    match eval cond with
    | VBool b -> if b then eval e1 else eval e2
    | _ -> failwith "Type error"
  )