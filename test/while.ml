let i = ref 0 in
fun f -> fun n -> i := n; while !i > 0 do f !i; i := (!i) - 1 done