module Make (IntLTS : Bilts.INT_LTS) = struct
  include Util.Monad.UserChooseWrite (struct
    type t = IntLTS.Int.Actions.Moves.move

    let show = IntLTS.Int.Actions.Moves.string_of_move
  end)

  let ask_print_conf pas_conf =
    print_endline "Do you want to print the Proponent configuration? (1/0)";
    let i = read_int () in
    match i with
    | 1 -> print_endline @@ IntLTS.string_of_passive_conf pas_conf
    | _ -> ()


  let rec generate conf =
    match conf with
    | IntLTS.Active act_conf ->
        let (action, pas_conf_option) = IntLTS.p_trans act_conf in
        begin
          match
            (pas_conf_option, action)
          with
          | (_, PDiv) ->
            print_endline @@ "Proponent has diverged ";
              return ()
          | (_, PError) ->
            print_endline @@ "Proponent has errored. Congratulation, you've found a bug! ";
              return ()
          | (None, Vis output_move) ->
            print_endline @@ "Proponent has quitted the game after playing "
            ^ IntLTS.Actions.Moves.string_of_move output_move;
              emit output_move
          | (Some pas_conf, Vis output_move) ->
              print_endline @@ "Proponent has played "
              ^ IntLTS.Actions.Moves.string_of_move output_move;
              let* () = emit output_move in
              generate (IntLTS.Passive pas_conf)
        end
    | IntLTS.Passive pas_conf ->
        ask_print_conf pas_conf;
        let results_list = IntLTS.M.run (IntLTS.o_trans_gen pas_conf) in
        print_endline "The possible moves are :";
        List.iter print_endline
          (List.map
             (fun (m, _) -> IntLTS.Actions.Moves.string_of_move m)
             results_list);
        let* (input_move, act_conf) = para_list results_list in
        print_endline @@ "You have played "
        ^ IntLTS.Actions.Moves.string_of_move input_move;
        let* () = emit input_move in
        generate (IntLTS.Active act_conf)

  let string_of_trace trace_list = List.map string_of_trace trace_list

  let get_traces act_conf =
    let result = generate act_conf in
    let trace_list = get_trace result in
    string_of_trace trace_list
end
