(* Paths in a typing LTS, indexed by the status of their initial and final
   positions; a play is a Player-ending path from a passive position. *)

module Make (TypingLTS : Typing.LTS) : sig
  type player_turn
  type opponent_turn

  (* A status is the pair of who is to move and who is not, so that the dual
     swaps it. *)
  type active = player_turn * opponent_turn
  type passive = opponent_turn * player_turn
  type _ status = Active : active status | Passive : passive status

  (* Both the weakening and the target are derived by check_move. *)
  type step = {
    move: TypingLTS.Moves.move;
    weakening: TypingLTS.Moves.Renaming.t;
    target: TypingLTS.position;
  }

  type ('s, 'e) path
  type t = (passive, passive) path
  type 's any_end = Any_end : ('s, 'e) path * 'e status -> 's any_end

  val path_of_actions :
    's status ->
    TypingLTS.position ->
    TypingLTS.Moves.pol_move list ->
    's any_end

  (* Nonempty. *)
  val actions_list_to_play :
    TypingLTS.position -> TypingLTS.Moves.pol_move list -> t

  val initial_position : ('s, 'e) path -> TypingLTS.position
  val final_position : ('s, 'e) path -> TypingLTS.position
  val actions : ('s, 'e) path -> TypingLTS.Moves.pol_move list
  val opponent_step : t -> (step * (active, passive) path) option
  val player_step : (active, passive) path -> step * t

  val extend_by_player_move :
    ('s, active) path -> TypingLTS.Moves.move -> ('s, passive) path

  (* None on the empty path. *)
  val drop_last_move : ('s, active) path -> ('s, passive) path option

  (* The same moves read by the other participant from [position], whose
     Player context is this path's Opponent context and whose Opponent
     context extends its Player context; None when a move does not type
     there. *)
  val dual :
    TypingLTS.position ->
    ('a * 'b, 'c * 'd) path ->
    ('b * 'a, 'd * 'c) path option
end = struct
  module Moves = TypingLTS.Moves
  module Namectx = Moves.Renaming.Namectx

  (* Two closed types, so that the two statuses are known to differ. *)
  type player_turn = [ `Player_turn ]
  type opponent_turn = [ `Opponent_turn ]
  type active = player_turn * opponent_turn
  type passive = opponent_turn * player_turn
  type _ status = Active : active status | Passive : passive status

  type step = {
    move: Moves.move;
    weakening: Moves.Renaming.t;
    target: TypingLTS.position;
  }

  type (_, _) steps =
    | End : ('s, 's) steps
    | Opponent_step : step * (active, 'e) steps -> (passive, 'e) steps
    | Player_step : step * (passive, 'e) steps -> (active, 'e) steps

  type ('s, 'e) path = { initial: TypingLTS.position; steps: ('s, 'e) steps }
  type t = (passive, passive) path
  type 's any_end = Any_end : ('s, 'e) path * 'e status -> 's any_end
  type 's any_steps = Any_steps : ('s, 'e) steps * 'e status -> 's any_steps

  let invalid_step position move =
    Util.Error.failwithf "Play: the move %a is not a valid step at %a"
      Moves.pp_move move TypingLTS.pp_position position

  let checked_step position ((_, move) as action) =
    match TypingLTS.check_move position action with
    | Some (weakening, target) -> { move; weakening; target }
    | None -> invalid_step position move

  let rec steps_of_actions : type s.
      s status -> TypingLTS.position -> Moves.pol_move list -> s any_steps =
   fun status position actions ->
    match (status, actions) with
    | (_, []) -> Any_steps (End, status)
    | (Passive, ((Moves.Input, _) as action) :: actions) ->
        let step = checked_step position action in
        let (Any_steps (steps, ending)) =
          steps_of_actions Active step.target actions in
        Any_steps (Opponent_step (step, steps), ending)
    | (Active, ((Moves.Output, _) as action) :: actions) ->
        let step = checked_step position action in
        let (Any_steps (steps, ending)) =
          steps_of_actions Passive step.target actions in
        Any_steps (Player_step (step, steps), ending)
    | (Passive, (Moves.Output, move) :: _) | (Active, (Moves.Input, move) :: _)
      ->
        invalid_step position move

  let path_of_actions status initial actions =
    let (Any_steps (steps, ending)) = steps_of_actions status initial actions in
    Any_end ({ initial; steps }, ending)

  let actions_list_to_play initial actions : t =
    match path_of_actions Passive initial actions with
    | Any_end ({ steps= End; _ }, _) ->
        failwith "Play.actions_list_to_play: an empty play is not Player-ending"
    | Any_end (play, Passive) -> play
    | Any_end (_, Active) ->
        failwith "Play.actions_list_to_play: the play is not Player-ending"

  let initial_position path = path.initial

  let rec final_position_of_steps : type s e.
      TypingLTS.position -> (s, e) steps -> TypingLTS.position =
   fun position -> function
    | End -> position
    | Opponent_step (step, steps) -> final_position_of_steps step.target steps
    | Player_step (step, steps) -> final_position_of_steps step.target steps

  let final_position path = final_position_of_steps path.initial path.steps

  let rec actions_of_steps : type s e. (s, e) steps -> Moves.pol_move list =
    function
    | End -> []
    | Opponent_step (step, steps) ->
        (Moves.Input, step.move) :: actions_of_steps steps
    | Player_step (step, steps) ->
        (Moves.Output, step.move) :: actions_of_steps steps

  let actions path = actions_of_steps path.steps

  let opponent_step (play : t) =
    match play.steps with
    | End -> None
    | Opponent_step (step, steps) -> Some (step, { initial= step.target; steps })

  let player_step (path : (active, passive) path) =
    match path.steps with
    | Player_step (step, steps) -> (step, { initial= step.target; steps })

  let rec extend_steps : type s.
      TypingLTS.position ->
      (s, active) steps ->
      Moves.move ->
      (s, passive) steps =
   fun position steps move ->
    match steps with
    | End -> Player_step (checked_step position (Moves.Output, move), End)
    | Opponent_step (step, steps) ->
        Opponent_step (step, extend_steps step.target steps move)
    | Player_step (step, steps) ->
        Player_step (step, extend_steps step.target steps move)

  let extend_by_player_move path move =
    { path with steps= extend_steps path.initial path.steps move }

  let rec drop_last_step : type s.
      (s, active) steps -> (s, passive) steps option = function
    | End -> None
    | Opponent_step (_, End) -> Some End
    | Opponent_step (step, steps) ->
        Option.map
          (fun steps -> Opponent_step (step, steps))
          (drop_last_step steps)
    | Player_step (step, steps) ->
        Option.map
          (fun steps -> Player_step (step, steps))
          (drop_last_step steps)

  let drop_last_move path =
    Option.map (fun steps -> { path with steps }) (drop_last_step path.steps)

  (* The inclusion maps this path's Player names to the other participant's
     Opponent names, extended at each Player move by its fresh names on both
     sides. *)
  let rec dual_steps : type a b c d.
      TypingLTS.position ->
      (Namectx.Names.name, Namectx.Names.name) Util.Pmap.pmap ->
      (a * b, c * d) steps ->
      (b * a, d * c) steps option =
   fun position inclusion steps ->
    let open Util.Monad.Option in
    match steps with
    | End -> return End
    | Opponent_step (step, steps) ->
        let move =
          Moves.map_free_names
            (fun name -> Util.Pmap.lookup_exn name inclusion)
            step.move in
        let* (weakening, target) =
          TypingLTS.check_move position (Moves.Output, move) in
        let* steps = dual_steps target inclusion steps in
        return (Player_step ({ move; weakening; target }, steps))
    | Player_step (step, steps) ->
        let* (weakening, target) =
          TypingLTS.check_move position (Moves.Input, step.move) in
        let inclusion =
          Util.Pmap.concat inclusion
            (Util.Pmap.list_to_pmap
               (List.combine
                  (Moves.fresh_names step.weakening step.move)
                  (Moves.fresh_names weakening step.move))) in
        let* steps = dual_steps target inclusion steps in
        return (Opponent_step ({ move= step.move; weakening; target }, steps))

  let dual position path =
    let inclusion =
      Util.Pmap.list_to_pmap
        (List.map
           (fun name -> (name, name))
           (Namectx.get_names (TypingLTS.get_namectxP path.initial))) in
    Option.map
      (fun steps -> { initial= position; steps })
      (dual_steps position inclusion path.steps)
end
