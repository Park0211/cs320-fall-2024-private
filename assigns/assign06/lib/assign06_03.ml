open Utils

let rec type_of (e: expr) : ty option =
  match e with
  | Num _-> Some TInt
  | Add (e1, e2) -> (
    match type_of e1, type_of e2 with
    | Some TInt, Some TInt -> Some TInt
    | _, _ -> None
  )
  | Lt (e1, e2) -> (
    match type_of e1, type_of e2 with
    | Some TInt, Some TInt -> Some TBool
    | _ -> None
  )
  | Ite (cond, e1, e2) -> (
    if type_of cond = Some TBool
      then (
        let e1_type = type_of e1 in
        if e1_type = type_of e2
          then e1_type
        else None
      )
      else None
  )
