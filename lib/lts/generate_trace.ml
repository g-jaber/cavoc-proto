module Make (IntLTS : Bipartite.LTS) = struct
  type conf = IntLTS.conf
  type move = IntLTS.Actions.Moves.move

  type event =
    | Trans of IntLTS.conf * IntLTS.Actions.Moves.move
    | Leaf of IntLTS.conf

  let string_of_event = function
    | Trans (_, move) ->
        (*IntLTS.Int.string_of_interactive_ctx ictx
          ^ " -" ^*)
        IntLTS.Actions.Moves.string_of_move move
        (*^ "-> "*)
    | Leaf _ -> "" (*IntLTS.Int.string_of_interactive_ctx ictx*)

  include Util.Monad.UserChooseWrite (struct
    type t = event

    let show = string_of_event
  end)

  let rec generate ~show_conf ~show_moves ~get_move conf =
    match conf with
    | IntLTS.Active act_conf ->
        let (action, pas_conf_option) = IntLTS.p_trans act_conf in
        begin
          match (pas_conf_option, action) with
          | (_, PDiv) ->
              print_endline @@ "Proponent has diverged ";
              let* () = emit @@ Leaf conf in
              return ()
          | (_, PError) ->
              print_endline
              @@ "Proponent has errored. Congratulation, you've found a bug! ";
              let* () = emit @@ Leaf conf in
              return ()
          | (None, Vis output_move) ->
              print_endline @@ "Proponent has quitted the game after playing "
              ^ IntLTS.Actions.Moves.string_of_move output_move;
              emit @@ Trans (conf, output_move)
          | (Some pas_conf, Vis output_move) ->
              print_endline @@ "Proponent has played "
              ^ IntLTS.Actions.Moves.string_of_move output_move;
              let* () = emit @@ Trans (conf, output_move) in
              generate ~show_conf ~show_moves ~get_move
                (IntLTS.Passive pas_conf)
        end
    | IntLTS.Passive pas_conf ->
        let pas_conf_str = IntLTS.string_of_passive_conf pas_conf in
        show_conf pas_conf_str;
        let results_list = IntLTS.M.run (IntLTS.o_trans_gen pas_conf) in
        let moves_list = List.map fst results_list in
        let string_list =
          List.map IntLTS.Actions.Moves.string_of_move moves_list in
        show_moves string_list;
        let chosen_index = get_move (List.length string_list) - 1 in
        let (input_move, act_conf) = List.nth results_list chosen_index in
        let* () = emit @@ Trans (conf, input_move) in
        generate ~show_conf ~show_moves ~get_move (IntLTS.Active act_conf)

  type graph = trace list

  let string_of_graph trace_list =
    String.concat "\n" @@ List.map string_of_trace trace_list

  let compute_graph ~show_conf ~show_moves ~get_move act_conf =
    let result = generate ~show_conf ~show_moves ~get_move act_conf in
    get_trace result
end
