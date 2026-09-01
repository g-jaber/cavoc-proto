let credit = ref 0
let insert n = if n > 0 then credit := !credit + n else ()
let buy () =
  (if !credit >= 1 then credit := !credit - 2 else ());
  assert (!credit >= 0)
