module type IBUILD = sig
  type conf
  
  (*Ajout du type result*)
  type result = Success | Stopped

  val interactive_build :
    show_move:(string -> unit) ->
    show_conf:(Yojson.Safe.t -> unit) ->
    show_moves_list:(Yojson.Safe.t list -> unit) ->
    (* the argument of get_move is the 
    number of moves *)
    get_move:(int -> int Lwt.t) ->
    conf ->
    result Lwt.t
end

module Make (IntLTS : Strategy.LTS) = struct
  type conf = IntLTS.conf
  type result = Success | Stopped

  let rec interactive_build
    ~show_move
    ~show_conf
    ~show_moves_list
    ~get_move
    conf =
  match conf with
  | IntLTS.Active act_conf ->
      begin
        match IntLTS.EvalMonad.run (IntLTS.p_trans act_conf) with
        | PropStop ->
            print_endline "Proponent has quitted the game.";
            Lwt.return Success (*Retourne un succés quand gagné*)
        | OpStop ->
            print_endline "Opponent has quitted the game.";
            Lwt.return Stopped
        | Continue (output_move, pas_conf) ->
            let move_string = IntLTS.TypingLTS.Moves.string_of_pol_move output_move in
            show_move move_string;
            interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
              (IntLTS.Passive pas_conf)
      end
    | IntLTS.Passive pas_conf ->
      let conf_json = IntLTS.passive_conf_to_yojson pas_conf in
      show_conf conf_json;
      let results_list =
        IntLTS.TypingLTS.BranchMonad.run (IntLTS.o_trans_gen pas_conf)
      in
      let moves_list = List.map (fun (x, _) -> x) results_list in

      (* JSON pour le front : id + label (+ payload local optionnel) *)
      let json_list =
        List.map IntLTS.TypingLTS.Moves.pol_move_to_yojson moves_list
      in

      show_moves_list json_list;
      let%lwt chosen_index = get_move (List.length json_list - 1) in

      let (input_move, act_conf) = List.nth results_list chosen_index in
      let move_string =
        IntLTS.TypingLTS.Moves.string_of_pol_move input_move
      in
      show_move move_string;
      interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
        (IntLTS.Active act_conf)
end