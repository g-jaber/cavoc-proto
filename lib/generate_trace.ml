module Make = functor (IntLTS : Bilts.INT_LTS) -> struct

  include Util.Monad.UserChooseWrite(struct 
    type t = (IntLTS.Int.Moves.move) 
    let show = IntLTS.Int.Moves.string_of_move 
  end)

let rec generate act_conf = 
  let (action,pas_conf_option) = IntLTS.p_trans act_conf in
  match (pas_conf_option,IntLTS.get_move_from_action action) with
    | (_,None) -> Util.Debug.print_debug "Stopping generation"; return ()
    | (None, Some output_move) -> 
      Util.Debug.print_debug "Stopping generation"; 
      emit output_move
    | (Some pas_conf,Some output_move) ->
      let* () = emit output_move in
      let results_list = IntLTS.M.run (IntLTS.o_trans_gen pas_conf) in
      print_endline "The possible moves are :";
      List.iter print_endline (List.map (fun (m,_) -> IntLTS.Int.Moves.string_of_move m) results_list);
      let* (input_move,act_conf') = para_list results_list in
      let* () = emit input_move in 
      (generate act_conf')

let string_of_trace trace_list =
  List.map string_of_trace trace_list

let get_traces act_conf =
  let result = generate act_conf in
  let trace_list = get_trace result in
  string_of_trace trace_list

end