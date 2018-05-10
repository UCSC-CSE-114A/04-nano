top =
  let even x = let odd x = if x == 0
                             then False
                             else even (x - 1)
               in
                 if x == 0
                   then True
                   else odd (x-1)
  in
    even 23
