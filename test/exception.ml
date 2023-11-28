(* To be tested with
./_build/default/bin/explore.exe test/exception.ml test/exception.mli
*)

exception Bla

let f b g = 
  try if b then (raise Bla) else g () with
    | x -> raise x