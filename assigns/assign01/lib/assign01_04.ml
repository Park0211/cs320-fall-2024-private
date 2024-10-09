open Assign01_01
open Assign01_02
open Assign01_03

let to_string s =
  let rec dec s i =
    let prime_i = nth_prime i in
    let exponent = nth s i in
    if exponent = 0 then [] 
    else exponent :: dec (s / (pow prime_i exponent)) (i + 1)
  in
  let l_to_s = function
    | [] -> "[]"
    | [x] -> "[" ^ string_of_int x ^ "]"
    | x :: xs -> "[" ^ String.concat "; " (List.map string_of_int (x :: xs)) ^ "]"
  in
  l_to_s (dec s 0)