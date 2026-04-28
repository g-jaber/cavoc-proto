module Make (IntLTS : Strategy.LTS) = struct
  type conf = IntLTS.conf

  include Util.Monad.Output (struct
    type t = string

    let show str = str
  end)

  let rec generate ~show_move ~show_conf ~show_moves_list ~get_move conf =
    match conf with
    | IntLTS.Active act_conf -> begin
        match IntLTS.EvalMonad.run (IntLTS.p_trans act_conf) with
        | PropStop ->
            print_endline
            @@ "Proponent has quitted the game after playing. Congratulation, \
                you won ! ";
            return ()
        | OpStop ->
            failwith
              "Opponent has stopped while it was not its turn. Please report."
        | Continue (output_move, pas_conf) ->
            let move_string =
              IntLTS.TypingLTS.Moves.string_of_pol_move output_move in
            show_move @@ "Proponent has played " ^ move_string;
            let* () = emit move_string in
            generate ~show_move ~show_conf ~show_moves_list ~get_move
              (IntLTS.Passive pas_conf)
      end
    | IntLTS.Passive pas_conf ->
        let conf_json = IntLTS.passive_conf_to_yojson pas_conf in
        let pas_conf_str = Yojson.Safe.pretty_to_string conf_json in
        show_conf pas_conf_str;
        let results_list =
          IntLTS.TypingLTS.BranchMonad.run (IntLTS.o_trans_gen pas_conf) in
        let moves_list = List.map (fun (x, _) -> x) results_list in
        let json_list =
          List.map IntLTS.TypingLTS.Moves.pol_move_to_yojson moves_list in
        show_moves_list json_list;
        let chosen_index = get_move (List.length json_list - 1) in
        let (input_move, act_conf) = List.nth results_list chosen_index in
        let move_string = IntLTS.TypingLTS.Moves.string_of_pol_move input_move in
        show_move @@ "You have chosen the move " ^ move_string;
        let* () = emit move_string in
        generate ~show_move ~show_conf ~show_moves_list ~get_move
          (IntLTS.Active act_conf)

  type graph = event list

  let string_of_graph trace_list =
    String.concat "\n" @@ List.map string_of_event trace_list

  let compute_graph ~show_move ~show_conf ~show_moves_list ~get_move act_conf =
    let result =
      generate ~show_move ~show_conf ~show_moves_list ~get_move act_conf in
    get_trace result
end
