module Make (IntLTS : Lts.Bipartite.INT_LTS) = struct
  module Graph = Lts.Graph.Make(IntLTS)

  (* Work in progress*)

  open Graph
  module Temporal = Temporal.Make(IntLTS.Int.IntLang.Store)
  open Temporal
  let translate_edge = function
  | PublicTrans (ActState (_,id), move, PasState (_,id')) -> 
    let kdata = IntLTS.Moves.get_kdata move in
    let store = IntLTS.Int.IntLang.get_store_of_a_nf kdata in
    let target_var = get_var id' in
    let formula = TImpl (PredStore store,Mod (EX, (Var target_var))) in
    (id,formula)
  | _ -> failwith ""

end