top =
  let f = \x -> \y -> \a -> a x * y in
  let g = \x -> x + 1 * 3           in
  f 7 8 g
