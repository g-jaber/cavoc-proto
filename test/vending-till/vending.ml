let insert n = if n > 0 then add n else ()
let buy () =
  (if covers 1 then sub 2 else ());
  assert (solvent ())
