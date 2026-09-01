let v = ref 0

let test x y z =
  if x || y then
    v := 1
  else if x then
    v := 2
  else if z then
    v := 3
  else
    v := 4
