let x = ref 0
let g f = x:=!x+1;f(); assert (!x != 5)