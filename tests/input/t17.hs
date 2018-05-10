top =
  let foldr f b xs = if xs == [] 
                       then b 
                       else let h = head xs in 
                            let t = tail xs in 
                              f h (foldr f b t) 
  in 
  let add x y = x + y 
  in
  let l = [1, 2, 3, 4] in
  foldr add 0 l 
