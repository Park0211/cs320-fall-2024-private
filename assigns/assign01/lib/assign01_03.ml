open Assign01_02

let nth s i =
  let ext_prime_pow s prime =
    let rec cnt_exp s prime count =
      if s mod prime = 0 then cnt_exp (s / prime) prime (count + 1)
      else count
    in
    cnt_exp s prime 0
  in
  let prime_i = nth_prime i in
  ext_prime_pow s prime_i