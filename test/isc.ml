let x = ref 0
let g f = x:=1;f();assert (!x = 1)