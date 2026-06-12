module type IBUILD = sig
  (* To be instanciated *)
  module M : Util.Monad.MONAD

  type conf

  (* *)

  val interactive_build :
    show_move:(string -> unit) ->
    show_conf:(Yojson.Safe.t -> unit) ->
    show_moves_list:(Yojson.Safe.t list -> unit) ->
    (* the argument of get_move is the 
    number of moves *)
    get_move:(int -> int M.m) ->
    conf ->
    string M.m
end

module type RUN_LTS = sig
  include Strategy.LTS

  module M : Util.Monad.MONAD

  val choose : (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.m -> (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.result M.m
end

module Make (M : Util.Monad.MONAD) (IntLTS : RUN_LTS with module M = M) = struct
  module M = M
  type conf = IntLTS.conf

  open M

  let rec interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
      conf =
    match conf with
    | IntLTS.Active act_conf -> begin
        let* res = IntLTS.choose (IntLTS.p_trans act_conf) in
        match res with
        | IntLTS.EvalMonad.PropStop ->
            return "Proponent has quitted the game.";
        | IntLTS.EvalMonad.PropDiverges ->
            return "The program diverges.";
        | IntLTS.EvalMonad.Continue (output_move, pas_conf) ->
            let move_string =
              IntLTS.TypingLTS.Moves.string_of_pol_move output_move in
            show_move move_string;
            interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
              (IntLTS.Passive pas_conf)
      end
    | IntLTS.Passive pas_conf ->
        let conf_json = IntLTS.passive_conf_to_yojson pas_conf in
        show_conf conf_json;
        let results_list =
          IntLTS.TypingLTS.BranchMonad.run (IntLTS.o_trans_gen pas_conf) in
        let moves_list = List.map (fun (x, _) -> x) results_list in

        (* JSON pour le front : id + label (+ payload local optionnel) *)
        let json_list =
          List.map IntLTS.TypingLTS.Moves.pol_move_to_yojson moves_list in

        show_moves_list json_list;
        let* chosen_index = get_move (List.length json_list - 1) in

        let (input_move, act_conf) = List.nth results_list chosen_index in
        let move_string = IntLTS.TypingLTS.Moves.string_of_pol_move input_move in
        let () = show_move move_string in
        interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
          (IntLTS.Active act_conf)
end
