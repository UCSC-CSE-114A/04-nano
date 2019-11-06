top =
  let fach c n = if 0 < n
                 then fach (c * n) (n - 1)
                 else c
  in
  let fac n = fach 1 n
  in
    fac 10
