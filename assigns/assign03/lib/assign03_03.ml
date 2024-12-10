open Assign03_list_util

type tree =
  | Leaf of int
  | Node of tree list


let rec height t =
  match t with
  | Leaf _ -> 0
  | Node cs ->
      let rec max_depth cs =
        match cs with
        | [] -> -1
        | c :: cs -> max (height c) (max_depth cs)
      in
      1 + max_depth cs

let collapse (h: int) (t: tree)=
  let rec collect_terminals t =
    match t with
    | Leaf _ -> [t]
    | Node [] -> [t]
    | Node cs -> 
        flatten (List.map collect_terminals cs)
  in

  let rec collapse_helper (t_h: int) (c_h: int) (t: tree) =
    match t with
    | Leaf _ -> t
    | Node [] -> t
    | Node cs ->
        if c_h = t_h - 1 then
          Node (collect_terminals t)
        else
          Node (List.map (fun child -> collapse_helper t_h (c_h + 1) child) cs)
  in

  collapse_helper h 0 t