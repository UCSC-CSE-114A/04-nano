top =
  let map f xs = if xs == [] 
                   then [] 
                   else let h = head xs in 
                        let t = tail xs in 
                          f h : map f t 
  in 
  let incr x = x + 1 
  in
  let l = [1, 2, 3, 4] in
  map incr l 
