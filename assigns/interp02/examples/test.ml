let rec a (n: int) : int =
  if n = 0 then 1
  else n * a (n - 1)

let _ : unit = assert (a 5 = 120)