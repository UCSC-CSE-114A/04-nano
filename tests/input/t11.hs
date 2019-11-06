top =
  let add n = if n == 0
              then 0
                else n + (add (n - 1))
                
  in
    add 10
