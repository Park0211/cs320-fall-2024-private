type 'a test = 
| TestCase of 'a
| TestList of 'a test list

let get_test_value t =
  match t with
  | TestCase a -> a
  | TestList _ -> failwith "get_test_value: not a test case"

let rec fold_left op (base: 'a) (test: 'b test) = 
  match test with
  | TestCase a -> op base a
  | TestList lst -> 
    match lst with
    | [] -> base
    | h::t ->
      match h with
      | TestCase h -> fold_left op (op base h) (TestList t)
      | TestList h -> fold_left op (fold_left op base (TestList h)) (TestList t)
