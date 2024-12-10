open Assign03_list_util

let update_value key new_value lst =
  List.map (fun (k, v) -> if k = key then (k, new_value) else (k, v)) lst


let mk_unique_keys alst =
  let rec mk_unique_keys_helper alst acc =
    match alst with
    | [] -> acc
    | (k, v) :: t ->
      if mem_assoc k acc
        then
          let v' = List.assoc k acc in
          let acc' = update_value k (v' + v) acc in
          mk_unique_keys_helper t acc'
      else mk_unique_keys_helper t ((k, v) :: acc)
  in
  mk_unique_keys_helper alst []
