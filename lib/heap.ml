let count_loc = ref 0
let fresh_loc () =
  let l = !count_loc in
  count_loc := !count_loc + 1; 
  l

(* Symbolic Heaps *)

type heap =  (Syntax.loc,Syntax.exprML) Pmap.pmap

let emptyheap = Pmap.empty

let allocate heap v = 
  let l = fresh_loc () in 
  (l,Pmap.add (l,v) heap)

let modify heap l value =
  Pmap.modadd_pmap (l,value) heap

let access heap l =
  Pmap.lookup_pmap l heap

let string_of_heap heap = 
  let heap' = Pmap.map_dom Syntax.string_of_loc heap in
  Pmap.string_of_pmap "↪" Syntax.string_of_exprML "ε" heap'