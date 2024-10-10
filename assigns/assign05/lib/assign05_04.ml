module ListSet = struct
  type t = int list
  let rec mem x s =
    match s with
    | [] -> false
    | h :: t ->
      if x = h then true
      else mem x t
  let empty = []
  let singleton x = [x]
  let rec card s =
    match s with
    | [] -> 0
    | _ :: t -> 1 + card t
  let union s1 s2 =
    let lst = List.sort (
      fun (a: 'a) (b: 'a) ->
        if (a > b) then 1
        else if (a < b) then -1
        else 0
    ) (s1 @ s2) in
    let rec unique lst =
      match lst with
      | [] -> []
      | [x] -> [x]
      | h1 :: (h2 :: t) ->
        if h1 = h2 then h1 :: unique t
        else h1 :: (h2 :: unique t)
      in
    unique lst
end

type set_info = {
  ind : int -> bool;
  mn : int;
  mx : int;
}

module FuncSet = struct
  type t = set_info
  let mem x s = s.ind x
  let empty = {
    ind = (fun _ -> false);
    mn = 0;
    mx = 0;
  }
  let singleton x = {
    ind = (fun y -> x = y);
    mn = x;
    mx = x;
  }
  let card s = s.mx - s.mn + 1
  let union s1 s2 = {
    ind = (fun x -> s1.ind x || s2.ind x);
    mn = min s1.mn s2.mn;
    mx = max s1.mx s2.mx;
  }
end
