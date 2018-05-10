top =
  let f = \g -> let x = 0 in g 2
  in
  let x = 100
  in
  let h = \ y -> x + y
  in
  f h
