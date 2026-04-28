module Make (M : Util.Monad.MONAD) (IntLTS : Strategy.LTS) = struct
  module M = M

  type conf = IntLTS.conf

  type result = Success | Stopped

  let rec interactive_build ~show_move ~show_conf ~show_moves_list ~get_move conf =
            let open M in
    match conf with
    | IntLTS.Active act_conf -> begin
        match IntLTS.EvalMonad.run (IntLTS.p_trans act_conf) with
        | PropStop ->
            print_endline
            @@ "Proponent has quitted the game after playing. Congratulation, \
                you won ! ";
            return Success
        | OpStop ->
            failwith
              "Opponent has stopped while it was not its turn. Please report."
        | Continue (output_move, pas_conf) ->
            let move_string =
              IntLTS.TypingLTS.Moves.string_of_pol_move output_move in
            show_move @@ "Proponent has played " ^ move_string;
            interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
              (IntLTS.Passive pas_conf)
      end
    | IntLTS.Passive pas_conf ->
        let conf_json = IntLTS.passive_conf_to_yojson pas_conf in
        show_conf conf_json;
        let results_list =
          IntLTS.TypingLTS.BranchMonad.run (IntLTS.o_trans_gen pas_conf) in
        let moves_list = List.map (fun (x, _) -> x) results_list in
        let json_list =
          List.map IntLTS.TypingLTS.Moves.pol_move_to_yojson moves_list in
        show_moves_list json_list;
        let* chosen_index = get_move (List.length json_list - 1) in
        let (input_move, act_conf) = List.nth results_list chosen_index in
        let move_string = IntLTS.TypingLTS.Moves.string_of_pol_move input_move in
        show_move @@ "You have chosen the move " ^ move_string;
        interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
          (IntLTS.Active act_conf)
end
