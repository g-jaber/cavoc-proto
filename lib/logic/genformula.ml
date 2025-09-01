module Make (IntLTS : Lts.Strategy.INT_LTS) = struct
  module Graph = Lts.Graph.Make(IntLTS)

  (* Work in progress*)

  open Graph
  module Temporal = Temporal.Make(IntLTS.Int.IntLang.Store)
  open Temporal
  let translate_edge = function
  | PublicTrans (ActState (_,id), (dir,move), PasState (_,id')) -> 
    let store = IntLTS.Int.IntLang.get_store_of_a_nf move in
    let target_var = get_var id' in
    let formula = TImpl (PredStore store,Mod (EX, (Var target_var))) in
    (id,formula)
  | _ -> failwith ""

end