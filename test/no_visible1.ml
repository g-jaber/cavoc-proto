(* dune exec bin/explore.exe -- -debug test/no_visible1.ml test/no_visible1.mli *)
let x = ref 0
let g f = 
    x := 0; 
    f (fun z -> x:= 0); 
    x:=1; 
    f (fun z -> z); 
    assert (!x = 1)