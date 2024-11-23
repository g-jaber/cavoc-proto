module Make (IntLTS : Bipartite.LTS) = struct
  type conf = IntLTS.conf

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
              generate (IntLTS.Passive pas_conf)
        end
    | IntLTS.Passive pas_conf ->
        ask_print_conf pas_conf;
        let results_list = IntLTS.M.run (IntLTS.o_trans_gen pas_conf) in
        print_endline "The possible moves are :";
        List.iter print_endline
          (List.mapi
             (fun i (m, _) ->
               string_of_int (i + 1)
               ^ ": "
               ^ IntLTS.Actions.Moves.string_of_move m)
             results_list);
        let* (input_move, act_conf) = para_list results_list in
        print_endline @@ "You have played "
        ^ IntLTS.Actions.Moves.string_of_move input_move;
        let* () = emit @@ Trans (conf, input_move) in
        generate (IntLTS.Active act_conf)

  type graph = trace list

  let string_of_graph trace_list =
    String.concat "\n" @@ List.map string_of_trace trace_list

  let compute_graph act_conf =
    let result = generate act_conf in
    get_trace result
end
