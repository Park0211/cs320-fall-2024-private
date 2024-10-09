open Assign03_list_util

let rec sum l =
  match l with
  | [] -> 0
  | h :: t -> h + sum t


let gen_fib l n =
  let len_l = List.length l in
  let rec gen_fib_helper i prev_values =
    let new_value = sum prev_values in
    let new_prev_values = (tail prev_values) @ [new_value] in
    if i = n then new_value
    else gen_fib_helper (i + 1) new_prev_values
  in
  if n < len_l then List.nth l n
  else gen_fib_helper len_l l