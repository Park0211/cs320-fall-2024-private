type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let sum_tr t = 
  let rec sum_tr_helper t cont =
    match t with
    | Leaf -> cont 0
    | Node (v, l, r) -> 
      sum_tr_helper l (
        fun l_sum -> 
          sum_tr_helper r (
            fun r_sum ->
            cont (v + l_sum + r_sum)
          )
        )
    in
  sum_tr_helper t (fun x -> x)