top =
  let add = \x -> if x <= 0
                    then 0
                    else (x + (add (x-1)))
  in
    add 10
