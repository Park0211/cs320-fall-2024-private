let head l =
  match l with
  | [] -> failwith "Empty list"
  | h :: _ -> h

let tail l =
  match l with
  | [] -> failwith "Empty list"
  | _ :: t -> t

let rec flatten lst =
  match lst with
  | [] -> []
  | h :: t -> h @ flatten t

let rec mem_assoc key lst =
  match lst with
  | [] -> false
  | (k, _) :: t -> if k = key then true else mem_assoc key t