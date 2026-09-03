(* The user may always leave the game rather than answer, so quitting is part
   of the interface of the loop. *)
type 'a user_action = Chose of 'a | Quit
type outcome = Prop_stopped | Prop_diverges | User_quit

let string_of_outcome = function
  | Prop_stopped -> "Proponent has quit the game."
  | Prop_diverges -> "The program diverges."
  | User_quit -> "You have quit the game."

(* Which side played a move, passed to show_move so a front-end can
   distinguish the two, e.g. by colour. *)
type player = Proponent | Opponent

module type IBUILD = sig
  (* To be instanciated *)
  module UserMonad : Util.Monad.MONAD

  type conf
  type pol_move

  (* *)

  val interactive_build :
    ?record_move:(pol_move -> unit) ->
    show_move:(player -> string -> unit) ->
    show_conf:(Yojson.Safe.t -> unit) ->
    show_moves_list:(Yojson.Safe.t list -> unit) ->
    (* the argument of get_move is the
    number of moves *)
    get_move:(int -> int user_action UserMonad.m) ->
    conf ->
    outcome UserMonad.m
end

module type RUN_LTS = sig
  include Strategy.LTS
  module UserMonad : Util.Monad.MONAD

  val choose :
    (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.m ->
    (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.result user_action
    UserMonad.m
end

module Make
    (UserMonad : Util.Monad.MONAD)
    (IntLTS : RUN_LTS with module UserMonad = UserMonad) =
struct
  module UserMonad = UserMonad

  type conf = IntLTS.conf
  type pol_move = IntLTS.TypingLTS.Moves.pol_move

  open UserMonad

  (* Opponent moves are supported by the Proponent name context, and
     conversely. *)
  let show_name_at pos =
    IntLTS.TypingLTS.Moves.Renaming.Namectx.show_name_in pos

  let rec interactive_build ?(record_move = fun _ -> ()) ~show_move ~show_conf
      ~show_moves_list ~get_move conf =
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
                (IntLTS.TypingLTS.get_namectxO (IntLTS.get_active_pos act_conf))
            in
            let (weakening, _) =
              IntLTS.TypingLTS.trigger_move
                (IntLTS.get_active_pos act_conf)
                output_move in
            let move_string =
              IntLTS.TypingLTS.Moves.string_of_pol_move_in ~show_name weakening
                output_move in
            show_move Proponent move_string;
            record_move output_move;
            interactive_build ~record_move ~show_move ~show_conf
              ~show_moves_list ~get_move (IntLTS.Passive pas_conf)
      end
    | IntLTS.Passive pas_conf -> (
        let conf_json = IntLTS.passive_conf_to_yojson pas_conf in
        show_conf conf_json;
        let show_name =
          show_name_at
            (IntLTS.TypingLTS.get_namectxP (IntLTS.get_passive_pos pas_conf))
        in
        let results_list =
          IntLTS.TypingLTS.BranchMonad.run (IntLTS.o_trans_gen pas_conf) in
        let weakening_of input_move =
          fst
            (IntLTS.TypingLTS.trigger_move
               (IntLTS.get_passive_pos pas_conf)
               input_move) in
        let json_list =
          List.map
            (fun (input_move, _) ->
              IntLTS.TypingLTS.Moves.pol_move_to_yojson_in ~show_name
                (weakening_of input_move) input_move)
            results_list in

        show_moves_list json_list;
        let* chosen = get_move (List.length json_list - 1) in
        match chosen with
        | Quit -> return User_quit
        | Chose chosen_index ->
            let (input_move, act_conf) = List.nth results_list chosen_index in
            let move_string =
              IntLTS.TypingLTS.Moves.string_of_pol_move_in ~show_name
                (weakening_of input_move) input_move in
            let () = show_move Opponent move_string in
            record_move input_move;
            interactive_build ~record_move ~show_move ~show_conf
              ~show_moves_list ~get_move (IntLTS.Active act_conf))
end
