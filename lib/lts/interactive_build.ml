(* The user may always leave the game rather than answer, so every callback
   consulting them returns a user_action. Quitting is thus part of the
   interface of the loop. *)
type 'a user_action =
  | Chose of 'a
  | Quit

type outcome =
  | Prop_stopped
  | Prop_diverges
  | User_quit

let string_of_outcome = function
  | Prop_stopped -> "Proponent has quit the game."
  | Prop_diverges -> "The program diverges."
  | User_quit -> "You have quit the game."

(* Which side played a move: the module (Proponent, on an Active configuration)
   or the user (Opponent, on a Passive one). Passed to show_move so a front-end
   can distinguish the two, e.g. by colour. *)
type player = Proponent | Opponent

module type IBUILD = sig
  (* To be instanciated *)
  module M : Util.Monad.MONAD

  type conf

  (* *)

  val interactive_build :
    show_move:(player -> string -> unit) ->
    show_conf:(Yojson.Safe.t -> unit) ->
    show_moves_list:(Yojson.Safe.t list -> unit) ->
    (* the argument of get_move is the
    number of moves *)
    get_move:(int -> int user_action M.m) ->
    conf ->
    outcome M.m
end

module type RUN_LTS = sig
  include Strategy.LTS

  module M : Util.Monad.MONAD

  val choose :
    (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.m ->
    (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.result user_action M.m
end

module Make (M : Util.Monad.MONAD) (IntLTS : RUN_LTS with module M = M) = struct
  module M = M
  type conf = IntLTS.conf

  open M

  (* Opponent moves are supported by the Proponent name context, and
     conversely. *)
  let show_name_at pos =
    IntLTS.TypingLTS.Moves.Renaming.Namectx.show_name_in pos

  let rec interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
      conf =
    match conf with
    | IntLTS.Active act_conf -> begin
        let* res = IntLTS.choose (IntLTS.p_trans act_conf) in
        match res with
        | Quit -> return User_quit
        | Chose IntLTS.EvalMonad.PropStop -> return Prop_stopped
        | Chose IntLTS.EvalMonad.PropDiverges -> return Prop_diverges
        | Chose (IntLTS.EvalMonad.Continue (output_move, pas_conf)) ->
            let show_name =
              show_name_at
                (IntLTS.TypingLTS.get_namectxO
                   (IntLTS.get_active_pos act_conf)) in
            let move_string =
              IntLTS.TypingLTS.Moves.string_of_pol_move_in ~show_name
                output_move in
            show_move Proponent move_string;
            interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
              (IntLTS.Passive pas_conf)
      end
    | IntLTS.Passive pas_conf ->
        let conf_json = IntLTS.passive_conf_to_yojson pas_conf in
        show_conf conf_json;
        let show_name =
          show_name_at
            (IntLTS.TypingLTS.get_namectxP (IntLTS.get_passive_pos pas_conf))
        in
        let results_list =
          IntLTS.TypingLTS.BranchMonad.run (IntLTS.o_trans_gen pas_conf) in
        let moves_list = List.map (fun (x, _) -> x) results_list in

        (* JSON pour le front : id + label (+ payload local optionnel) *)
        let json_list =
          List.map
            (IntLTS.TypingLTS.Moves.pol_move_to_yojson_in ~show_name)
            moves_list in

        show_moves_list json_list;
        let* chosen = get_move (List.length json_list - 1) in
        match chosen with
        | Quit -> return User_quit
        | Chose chosen_index ->
            let (input_move, act_conf) = List.nth results_list chosen_index in
            let move_string =
              IntLTS.TypingLTS.Moves.string_of_pol_move_in ~show_name
                input_move in
            let () = show_move Opponent move_string in
            interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
              (IntLTS.Active act_conf)
end
