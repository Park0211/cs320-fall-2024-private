let is_prime n =
  let rec check_divisor d =
    if d * d > n then true
    else if n mod d = 0 then false
    else check_divisor (d + 1)
  in
  if n < 2 then false else check_divisor 2

let nth_prime n =
  let rec find_prime count candidate =
    if count = n then candidate - 1
    else if is_prime candidate then find_prime (count + 1) (candidate + 1)
    else find_prime count (candidate + 1)
  in
  find_prime 0 2