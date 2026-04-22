let x = ref 0
let test f = let y = !x in f (); assert (!x = y); x := x+1