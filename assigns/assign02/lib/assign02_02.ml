type matrix = {
  entries : float list list;
  rows : int;
  cols : int;
}

let mk_matrix (l: float list) (r, c) =
  let rec row_split (l: float list) (mtx: float list list) (r: int) (c: int) =
    let rec col_split (l: float list) (c: int) (r: float list) =
      if c = 0
      then (l, List.rev r)
      else
        match l with
          | [] -> ([], r)
          | h :: t -> col_split t (c - 1) (h :: r)
      in

    if r = 0
    then mtx
    else 
      match l with
        | [] -> mtx
        | l -> begin
          let (l, row) = col_split l c [] in
          row_split l (mtx @ [row]) (r - 1) c
        end
  in
  
  {
    entries = row_split l [] r c;
    rows = r;
    cols = c
  }
