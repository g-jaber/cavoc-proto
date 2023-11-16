exception Bla

let f b = 
  try if b then (raise Bla) else true with
    | x -> raise x