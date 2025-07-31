(* To be tested with
dune exec bin/explore.exe test/exception.ml test/exception.mli
*)

exception Bla of unit

let f b g = 
  try if b then (raise (Bla ())) else g () with
    | x -> raise x