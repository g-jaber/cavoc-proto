(* This module provides a way to synchronize the interaction between the interaction of two configurations of the same bipartite LTS.
   One of the configuration must be active, and the other one is passive.
   *)

module Make = functor (IntLTS : Bilts.INT_LTS) -> struct

include Util.Monad.LWMonad(struct type t = (IntLTS.Int.Actions.action) end)

let rec synchronize nspan confP confO =
  Util.Debug.print_debug "One step of synchronization";
  let (act_conf,pas_conf,act_player) = match (confP,confO) with 
    | (IntLTS.Active aconf,IntLTS.Passive pconf) -> (aconf,pconf,IntLTS.Int.Moves.Proponent) 
    | (IntLTS.Passive pconf,IntLTS.Active aconf) -> (aconf,pconf,IntLTS.Int.Moves.Opponent)
    | (IntLTS.Active _, IntLTS.Active _) -> failwith "Error: trying to synchronize two active configurations. Please report."
    | (IntLTS.Passive _, IntLTS.Passive _) -> failwith "Error: trying to synchronize two passive configurations. Please report."
  in
  let (act_move,pas_conf_option) = IntLTS.p_trans act_conf in
  match pas_conf_option with
    | None -> Util.Debug.print_debug "Stopping synchronization"; print act_move
    | Some pas_conf' ->
      let* (pas_move,act_conf') = para_list (IntLTS.M.run (IntLTS.o_trans pas_conf)) in
      let (pmove,omove,confP',confO') = begin match act_player with
        | IntLTS.Int.Moves.Proponent -> (act_move,pas_move,IntLTS.Active act_conf',IntLTS.Passive pas_conf')
        | IntLTS.Int.Moves.Opponent -> (pas_move,act_move,IntLTS.Passive pas_conf',IntLTS.Active act_conf')
      end in
      Util.Debug.print_debug ("Synching moves " ^ (IntLTS.Int.Actions.string_of_action pmove) ^" and " ^ (IntLTS.Int.Actions.string_of_action omove));
      let nspan_option = IntLTS.Int.Actions.synch_action nspan pmove omove in
      begin match nspan_option with
        | None -> Util.Debug.print_debug ("Synching failed in namespan " ^ (Util.Pmap.string_of_pmap "" "," IntLTS.Int.Actions.Lang.string_of_name IntLTS.Int.Actions.Lang.string_of_name) nspan);
          return ()
        | Some nspan' -> Util.Debug.print_debug "Synching succeeded"; let* () = print pmove in (synchronize nspan' confP' confO')
      end

(* TODO: it would make more sense to synchronize by first retrieving the action performed by the active configuration,
   then checking if the dual action is a valid one for the passive configuration. *)

let get_traces namespan act_conf pas_conf =
  let result = synchronize namespan act_conf pas_conf in
  let trace_list = get_output result in
  List.map (fun outputl -> String.concat "," @@ List.map IntLTS.Int.Actions.string_of_action outputl) trace_list
end