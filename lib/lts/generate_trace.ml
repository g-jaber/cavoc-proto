module Make (IntLTS : Bipartite.LTS) = struct
  type conf = IntLTS.conf
  type move = IntLTS.Moves.move

  type event =
    | Trans of IntLTS.conf * IntLTS.Moves.move

  let string_of_event = function
    | Trans (_, move) ->
        (*IntLTS.Int.string_of_position ictx
          ^ " -" ^*)
        IntLTS.Moves.string_of_move move
        (*^ "-> "*)

  include Util.Monad.UserChooseWrite (struct
    type t = event

    let show = string_of_event
  end)

  let rec generate ~show_conf ~show_moves_list ~get_move conf =
    match conf with
    | IntLTS.Active act_conf -> begin
        match IntLTS.EvalMonad.run (IntLTS.p_trans act_conf) with
        | None ->
            print_endline
            @@ "Proponent has quitted the game after playing. Congratulation, \
                you won ! ";
            return ()
        | Some ((output_move,_), pas_conf) ->
            print_endline @@ "Proponent has played "
            ^ IntLTS.Moves.string_of_move output_move;
            let* () = emit @@ Trans (conf, output_move) in
            generate ~show_conf ~show_moves_list ~get_move
              (IntLTS.Passive pas_conf)
      end
    | IntLTS.Passive pas_conf ->
        let conf_json = IntLTS.passive_conf_to_yojson pas_conf in
        let pas_conf_str = Yojson.Safe.pretty_to_string conf_json in
        show_conf pas_conf_str;
        let results_list = IntLTS.OBranchingMonad.run (IntLTS.o_trans_gen pas_conf) in
        let moves_list = List.map (fun ((x,_),_) -> x) results_list in
        let string_list =
          List.map IntLTS.Moves.string_of_move moves_list in
        show_moves_list string_list;
        let chosen_index = get_move (List.length string_list) - 1 in
        let ((input_move,_), act_conf) = List.nth results_list chosen_index in
        let* () = emit @@ Trans (conf, input_move) in
        generate ~show_conf ~show_moves_list ~get_move (IntLTS.Active act_conf)

  type graph = trace list

  let string_of_graph trace_list =
    String.concat "\n" @@ List.map string_of_trace trace_list

  let compute_graph ~show_conf ~show_moves_list ~get_move act_conf =
    let result = generate ~show_conf ~show_moves_list ~get_move act_conf in
    get_trace result
end
