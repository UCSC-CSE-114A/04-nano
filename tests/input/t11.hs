top =
  let addh c n = if 0 < n
                 then addh (c + n) (n - 1) 
                 else c
  in
  let add n = addh 0 n
  in
    add 10
