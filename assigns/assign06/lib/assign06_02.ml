open Utils

let parse (lst: tok list) : expr option =
  let rec parse_helper (lst: tok list) (acc: expr list) =
    match lst with
      | [] -> (
        match acc with
        | [e] -> Some e
        | _ -> None
      )
      | h :: t -> (
        match h with
        | TNum n -> parse_helper t (Num n :: acc)
        | TAdd -> (
          match acc with
          | e1::e2::rest -> parse_helper t (Add (e2, e1)::rest)
          | _ -> None
        )
        | TLt -> (
          match acc with
          | e1::e2::rest -> parse_helper t (Lt (e2, e1)::rest)
          | _ -> None
        )
        | TIte -> (
          match acc with
          | e1::e2::cond::rest -> parse_helper t (Ite (cond, e2, e1)::rest)
          | _ -> None
        )
      )
  in
  parse_helper lst []
