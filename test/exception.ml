(* To be tested with
dune exec explore.exe test/exception.ml test/exception.mli
*)

exception Bla

let f b g = 
  try if b then (raise Bla) else g () with
    | x -> raise x