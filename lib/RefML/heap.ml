(* Concrete Heaps *)

type heap = (Syntax.loc, Syntax.value) Util.Pmap.pmap

let emptyheap = Util.Pmap.empty

let allocate heap v =
  let l = Syntax.fresh_loc () in
  (l, Util.Pmap.add (l, v) heap)

let modify heap l value = Util.Pmap.modadd (l, value) heap
let update heap heap' =
  Util.Pmap.fold (fun heap (l,value) -> modify heap l value) heap heap'

let lookup heap l = Util.Pmap.lookup l heap

let string_of_heap =
  Util.Pmap.string_of_pmap "[]" "->" Syntax.string_of_loc Syntax.string_of_term


let loc_ctx_of_heap = Util.Pmap.map_im (fun _ -> Types.TInt)
(* TODO !!! *)

let rec shuffle_heaps = function
  | [] -> [ emptyheap ]
  | (loc, listval) :: tl ->
      let heaplist = shuffle_heaps tl in
      let aux value = List.map (Util.Pmap.add (loc, value)) heaplist in
      List.flatten (List.map aux listval)

let generate_heaps loc_ctx =
  Util.Debug.print_debug @@ "Generating heap for " ^ Type_ctx.string_of_loc_ctx loc_ctx;
  let list_predheap =
    Util.Pmap.map_list
      (fun (l, ty) -> (l, Syntax.generate_ground_value ty))
      loc_ctx in
  shuffle_heaps list_predheap

let restrict loc_ctx heap =
  Util.Pmap.filter_dom (fun l -> Util.Pmap.mem l loc_ctx) heap