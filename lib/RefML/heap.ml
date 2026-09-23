(* Concrete Heaps *)
type heap = (Syntax.loc, Syntax.value) Util.Pmap.pmap

let pp_heap fmt heap =
  let pp_pair fmt (l,v) = Format.fprintf fmt "%a ↪ %a" Syntax.pp_loc l Syntax.pp_value v in
  let pp_sep fmt () = Format.fprintf fmt ";" in
  let pp_heap_aux = Util.Pmap.pp_pmap ~pp_sep pp_pair in
  Format.fprintf fmt "[%a]" pp_heap_aux heap
  

let string_of_heap =
  Format.asprintf "%a" pp_heap

let heap_to_yojson heap = 
  let heap_l = Util.Pmap.to_list heap in
  let heap_l' = List.map (fun (l,v) -> (Syntax.string_of_loc l,Syntax.value_to_yojson v)) heap_l in
  `Assoc heap_l'

let emptyheap = Util.Pmap.empty

let allocate heap v =
  let l = Syntax.fresh_loc () in
  (l, Util.Pmap.add (l, v) heap)

let modify heap l value = Util.Pmap.modadd (l, value) heap

let update heap heap' =
  Util.Pmap.fold (fun heap (l,value) -> modify heap l value) heap heap'

let lookup heap l = Util.Pmap.lookup l heap

(* The most recent allocation comes first in the heap. *)
let loc_ctx_of_heap heap =
  Util.Pmap.list_to_pmap
    (List.rev
       (Util.Pmap.to_list
          (Util.Pmap.filter_map_im Syntax.type_of_ground_value heap)))
