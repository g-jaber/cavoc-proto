let c = ref 0
let inc () = c:=!c+1
let get () = assert (!c < 5); !c