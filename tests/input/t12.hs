top =
  let foldn f b n = let loop i c = if i <= n
                                     then (loop (i+1)) ((f i) c)
                                     else c
                    in
                        (loop 0) b
  in
  let fac = (foldn (\x -> \y -> (if x == 0 then 1 else (x * y)))) 1 
  in
    fac 10
