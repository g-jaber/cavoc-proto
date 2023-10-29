(* This module provides a way to synchronize the interaction between the interaction of two configurations of the same bipartite LTS.
   One of the configuration must be active, and the other one is passive.
   *)

module Make = functor (IntLTS : Bilts.INT_LTS) -> struct

include Util.Monad.UserChooseWrite(struct 
    type t = (IntLTS.Int.Moves.move) 
    let show = IntLTS.Int.Moves.string_of_move 
  end)

type player = Proponent | Opponent

let rec synchronize_check confP confO =
  Util.Debug.print_debug "One step of synchronization";
  let (act_conf,pas_conf,act_player) = match (confP,confO) with 
    | (IntLTS.Active aconf,IntLTS.Passive pconf) -> (aconf,pconf,Proponent) 
    | (IntLTS.Passive pconf,IntLTS.Active aconf) -> (aconf,pconf,Opponent)
    | (IntLTS.Active _, IntLTS.Active _) -> failwith "Error: trying to synchronize two active configurations. Please report."
    | (IntLTS.Passive _, IntLTS.Passive _) -> failwith "Error: trying to synchronize two passive configurations. Please report."
  in
  let (action,pas_conf_option) = IntLTS.p_trans act_conf in
  match (pas_conf_option,IntLTS.get_move_from_action action) with
    | (_,None) -> Util.Debug.print_debug "Stopping synchronization"; return ()
    | (None, Some move) -> Util.Debug.print_debug "Stopping synchronization"; emit move
    | (Some pas_conf',Some output_move) ->
      let input_move = IntLTS.Int.Moves.switch_direction output_move in
      begin match IntLTS.o_trans pas_conf input_move with
      | None ->
        Util.Debug.print_debug ("Input move forbidden: " ^ (IntLTS.Int.Moves.string_of_move input_move)
          ^ " in the configuration " ^ IntLTS.string_of_passive_conf pas_conf);
        emit output_move
      | Some act_conf' -> 
        let (moveP,confP',confO') = begin match act_player with
          | Proponent -> (output_move,IntLTS.Passive pas_conf',IntLTS.Active act_conf')
          | Opponent -> (input_move,IntLTS.Active act_conf',IntLTS.Passive pas_conf')
        end in
        Util.Debug.print_debug @@ "Synching succeeded with move " ^ (IntLTS.Int.Moves.string_of_move moveP); 
        let* () = emit moveP in (synchronize_check confP' confO')
      end
let rec synchronize_gen nspan confP confO =
  Util.Debug.print_debug "One step of synchronization";
  let (act_conf,pas_conf,act_player) = match (confP,confO) with 
    | (IntLTS.Active aconf,IntLTS.Passive pconf) -> (aconf,pconf,Proponent) 
    | (IntLTS.Passive pconf,IntLTS.Active aconf) -> (aconf,pconf,Opponent)
    | (IntLTS.Active _, IntLTS.Active _) -> failwith "Error: trying to synchronize two active configurations. Please report."
    | (IntLTS.Passive _, IntLTS.Passive _) -> failwith "Error: trying to synchronize two passive configurations. Please report."
  in
  let (action,pas_conf_option) = IntLTS.p_trans act_conf in
  match (pas_conf_option,IntLTS.get_move_from_action action) with
    | (_,None) -> Util.Debug.print_debug "Stopping synchronization"; return ()
    | (None, Some move) -> Util.Debug.print_debug "Stopping synchronization"; emit move
    | (Some pas_conf',Some output_move) ->
      let* (input_move,act_conf') = para_list (IntLTS.M.run (IntLTS.o_trans_gen pas_conf)) in
      let (moveP,moveO,confP',confO') = begin match act_player with
        | Proponent -> (output_move,input_move,IntLTS.Passive pas_conf',IntLTS.Active act_conf')
        | Opponent -> (input_move,output_move,IntLTS.Active act_conf',IntLTS.Passive pas_conf')
      end in
      Util.Debug.print_debug ("Synching moves " ^ (IntLTS.Int.Moves.string_of_move moveP) ^" and " ^ (IntLTS.Int.Moves.string_of_move moveO));
      let nspan_option = IntLTS.Int.Actions.synch_move nspan moveP moveO in
      begin match nspan_option with
        | None -> 
          let span_string = Util.Namespan.string_of_span IntLTS.Int.Actions.Lang.string_of_name nspan in
          Util.Debug.print_debug ("Synching failed in namespan " ^ span_string);
          return ()
        | Some nspan' -> 
          Util.Debug.print_debug @@ "Synching succeeded with move " ^ (IntLTS.Int.Moves.string_of_move moveP); 
          let* () = emit moveP in (synchronize_gen nspan' confP' confO')
      end

(* TODO: it would make more sense to synchronize by first retrieving the action performed by the active configuration,
   then checking if the dual action is a valid one for the passive configuration. *)

let get_traces_check act_conf pas_conf =
  let result = synchronize_check act_conf pas_conf in
  let trace_list = get_trace result in
  List.map string_of_trace trace_list

let get_traces_gen namespan act_conf pas_conf =
  let result = synchronize_gen namespan act_conf pas_conf in
  let trace_list = get_trace result in
  List.map string_of_trace trace_list
end