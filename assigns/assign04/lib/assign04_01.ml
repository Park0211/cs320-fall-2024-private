let fst (x, _) = x

let hd l =
  match l with
  | [] -> failwith "Empty list"
  | h :: _ -> h

let get_v x =
  match x with
  | Some v -> v
  | None -> failwith "None"

let last_function_standing (funcs: ('a -> 'a) list) (start: 'a) (pred: ('a -> bool)) =
  let rec lifespan (func: ('a -> 'a)) (v: 'a) (acc: int) =
    let next_v = func(v) in
    if pred(next_v) then
      acc + 1
    else
      lifespan func next_v acc + 1
    in
  let rec last_function_standing_helper funcs start pred acc =
    match funcs with
    | [] -> acc
    | f :: fs ->
        let current = lifespan f start 0 in
        let max = fst (get_v acc) in
        if current > max then
          last_function_standing_helper fs start pred (Some (current, f))
        else if current = max then
          None
        else
          last_function_standing_helper fs start pred acc
      in
  if funcs = [] then
    None
  else
    match (last_function_standing_helper funcs start pred (Some (0, hd funcs))) with
    | Some (_, f) -> Some f
    | None -> None
