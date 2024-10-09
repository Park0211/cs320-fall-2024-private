type piece = 
  | X
  | O

type pos = 
  | Piece of piece
  | Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = 
  | Top
  | Middle
  | Bottom

type col_index = 
  | Left
  | Middle
  | Right

type pos_index = row_index * col_index

let get_pos b p_i =
  match p_i with
    | (Top, Left) -> let ((a, _, _), _, _) = b in a
    | (Top, Middle) -> let ((_, a, _), _, _) = b in a
    | (Top, Right) -> let ((_, _, a), _, _) = b in a
    | (Middle, Left) -> let (_, (a, _, _), _) = b in a
    | (Middle, Middle) -> let (_, (_, a, _), _) = b in a
    | (Middle, Right) -> let (_, (_, _, a), _) = b in a
    | (Bottom, Left) -> let (_, _, (a, _, _)) = b in a
    | (Bottom, Middle) -> let (_, _, (_, a, _)) = b in a
    | (Bottom, Right) -> let (_, _, (_, _, a)) = b in a
;;

let winner b = 
  let check_win = function
    | (Piece X, Piece X, Piece X) -> true
    | (Piece O, Piece O, Piece O) -> true
    | _ -> false
  in

  let ((a, b, c), (d, e, f), (g, h, i)) = b in
  if check_win (a, b, c) then true
  else if check_win (d, e, f) then true
  else if check_win (g, h, i) then true
  else if check_win (a, d, g) then true
  else if check_win (b, e, h) then true
  else if check_win (c, f, i) then true
  else if check_win (a, e, i) then true
  else if check_win (c, e, g) then true
  else false
;;