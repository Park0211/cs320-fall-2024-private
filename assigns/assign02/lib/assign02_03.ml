type dir = 
| North
| South
| East
| West

type path = dir list

let dist (p: path) =
  let rec inner_dist (p: path) (x: int) (y: int) =
    match p with
    | [] -> (x, y)
    | h :: remaining_path -> begin
      match h with
      | North -> inner_dist remaining_path x (y + 1)
      | South -> inner_dist remaining_path x (y - 1)
      | East -> inner_dist remaining_path (x + 1) y
      | West -> inner_dist remaining_path (x - 1) y
    end
  in
  let (x, y) = inner_dist p 0 0 in
  sqrt (float_of_int (x * x + y * y))
;;
