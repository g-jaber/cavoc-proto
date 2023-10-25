(* Concrete Heaps *)

type heap =  (Syntax.loc,Syntax.exprML) Util.Pmap.pmap

let emptyheap = Util.Pmap.empty

let allocate heap v = 
  let l = Syntax.fresh_loc () in 
  (l,Util.Pmap.add (l,v) heap)

let modify heap l value =
  Util.Pmap.modadd_pmap (l,value) heap

let access heap l =
  Util.Pmap.lookup l heap

let string_of_heap = 
  Util.Pmap.string_of_pmap "ε" "↪" Syntax.string_of_loc Syntax.string_of_exprML

let loc_ctx_of_heap = 
  Util.Pmap.map_im (fun _ -> Types.TInt)

let rec shuffle_heaps  = function
  | [] -> [emptyheap]
  | (loc,listval)::tl -> 
    let heaplist = shuffle_heaps tl in
    let aux value =
      List.map (Util.Pmap.add (loc,value)) heaplist
    in List.flatten (List.map aux listval)
   
let generate_heaps loc_ctx = 
  let list_predheap =
  Util.Pmap.map_list (fun (l,ty) -> (l,Syntax.generate_ground_value  ty)) loc_ctx
  in shuffle_heaps list_predheap