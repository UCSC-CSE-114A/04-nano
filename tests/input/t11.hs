top =
  let foldn f b n = let loop i c = if i <= n
                                     then (loop (i+1)) ((f i) c)
                                     else c
                    in
                        (loop 0) b
  in
  let add = (foldn (\x -> \y -> x + y)) 0
  in
    add 10
