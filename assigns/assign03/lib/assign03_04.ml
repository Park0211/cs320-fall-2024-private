open Assign03_list_util

type sign =
  | Pos
  | Neg
  | Zero


let get_sign n =
  if n > 0 then
    Pos
  else if n < 0 then
    Neg
  else
    Zero

let opposite_sign s = 
  match s with
  | Pos -> Neg
  | Neg -> Pos
  | Zero -> Zero

let group l =
  let rec group_helper l prev_sign acc =
    match l with
    | [] -> Some (List.rev acc)
    | h :: t ->
      let current_sign = get_sign h in
      if current_sign = prev_sign then
        match acc with
          | [] -> group_helper t prev_sign [[h]]
          | h' :: t' -> group_helper t prev_sign ((h' @ [h]) :: t')
      else if current_sign = Zero then
        match acc with
          | [] -> group_helper t (opposite_sign prev_sign) []
          | _ -> group_helper t (opposite_sign prev_sign) ([] :: acc)
      else None
  in

  let first_sign = get_sign (head l) in
  match first_sign with
  | Zero -> None
  | _ -> group_helper l first_sign []