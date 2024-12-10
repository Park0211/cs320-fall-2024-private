let rec go = fun n ->
  if n <= 1
    then n
  else go (n - 1) + go (n - 2)
in go 8