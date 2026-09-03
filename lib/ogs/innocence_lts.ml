(* The innocence restriction over a typing LTS: the O-move after a P-move 
is the one the play so far records at the same O-view, if it exists. *)
module Restriction
    (A_nf : Lang.Interactive.A_NF)
    (TypingLTS :
      Lts.Typing.LTS
        with module Moves.Renaming = A_nf.IEnv.Renaming
         and type Moves.copattern =
          A_nf.abstract_normal_form * A_nf.IEnv.Renaming.Namectx.t)
    (ExtraMemory :
      Lts.Extra_memory.EXTRA_MEMORY
        with type move = TypingLTS.Moves.move
         and type name = TypingLTS.Moves.Renaming.Namectx.Names.name
         and type namectx = TypingLTS.Moves.Renaming.Namectx.t
         and type renaming = TypingLTS.Moves.Renaming.t) :
  Vis_lts.RESTRICTION with module TypingLTS = TypingLTS = struct
  module TypingLTS = TypingLTS
  module Moves = TypingLTS.Moves
  module ViewFunction = Lts.View_function.Make (A_nf) (TypingLTS) (ExtraMemory)

  (* The Opponent initial move is not recorded: the recording starts at the
     position after it. *)
  type conf =
    | Initial
    | Active of ViewFunction.recording
    | Passive of ViewFunction.recording * ViewFunction.localized_opponent_move

  let pp_argument fmt (localized : ViewFunction.localized_opponent_move) =
    ViewFunction.pp_argument fmt localized.argument

  let conf_to_yojson = function
    | Initial -> `Null
    | Active recording -> ViewFunction.recording_to_yojson recording
    | Passive (recording, localized) ->
        `Assoc
          [
            ("recording", ViewFunction.recording_to_yojson recording);
            ("argument", `String (Format.asprintf "%a" pp_argument localized));
          ]

  let pp_conf fmt = function
    | Initial -> Format.pp_print_string fmt "Initial"
    | Active recording -> ViewFunction.pp_recording fmt recording
    | Passive (recording, localized) ->
        Format.fprintf fmt "@[⟨%a |@, Argument: %a⟩@]" ViewFunction.pp_recording
          recording pp_argument localized

  let init_active_conf namectxP namectxO =
    Active (ViewFunction.initial_recording namectxO namectxP)

  let init_passive_conf _namectxP _namectxO = Initial

  let check target_position weakening conf (direction, move) =
    match (conf, direction) with
    | (Initial, Moves.Input) ->
        Some
          (Active
             (ViewFunction.initial_recording
                (TypingLTS.get_namectxO target_position)
                (TypingLTS.get_namectxP target_position)))
    | (Active recording, Moves.Output) ->
        let (localized, recording) =
          ViewFunction.localize_opponent_step recording
            (TypingLTS.get_namectxP target_position)
            weakening move in
        Some (Passive (recording, localized))
    | (Passive (recording, localized), Moves.Input) ->
        Option.map
          (fun recording -> Active recording)
          (ViewFunction.record_player_step recording localized weakening move)
    | _ -> None
end

module Make
    (A_nf : Lang.Interactive.A_NF)
    (TypingLTS :
      Lts.Typing.LTS
        with module Moves.Renaming = A_nf.IEnv.Renaming
         and type Moves.copattern =
          A_nf.abstract_normal_form * A_nf.IEnv.Renaming.Namectx.t) =
  Vis_lts.Make
    (Restriction (A_nf) (TypingLTS)
       (Lts.Extra_memory.InnocentMachine (TypingLTS.Moves)))
