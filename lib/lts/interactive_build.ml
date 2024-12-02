module type IBUILD = sig
  (* To be instanciated *)
  type conf

  (* *)

  val interactive_build : 
  show_conf:(string -> unit) -> 
  show_moves:(string list -> unit) -> 
    (* the argument of get_move is the 
    number of moves *)
    get_move:(int -> int Lwt.t) 
  -> conf -> unit Lwt.t
end

module Make (IntLTS : Bipartite.LTS) = struct
  type conf = IntLTS.conf


  let rec interactive_build ~show_conf ~show_moves ~get_move conf =
    match conf with
    | IntLTS.Active act_conf ->
        let (action, pas_conf_option) = IntLTS.p_trans act_conf in
        begin
          match (pas_conf_option, action) with
          | (_, PDiv) ->
              Lwt.return ()
          | (_, PError) ->
              print_endline
              @@ "Proponent has errored. Congratulation, you've found a bug! ";
              Lwt.return ()
          | (None, Vis output_move) ->
              print_endline @@ "Proponent has quitted the game after playing "
              ^ IntLTS.Actions.Moves.string_of_move output_move;
              Lwt.return ()
          | (Some pas_conf, Vis output_move) ->
              print_endline @@ "Proponent has played "
              ^ IntLTS.Actions.Moves.string_of_move output_move;
              interactive_build ~show_conf ~show_moves ~get_move
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
        let%lwt chosen_index = get_move @@ (List.length string_list) - 1 in
        let (_, act_conf) = List.nth results_list chosen_index in
        interactive_build ~show_conf ~show_moves ~get_move (IntLTS.Active act_conf)
end