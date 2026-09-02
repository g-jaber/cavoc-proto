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

  (* Every target position is derived by check_move. *)
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

  val opponent_step :
    t ->
    (TypingLTS.Moves.move * TypingLTS.position * (active, passive) path) option

  val player_step :
    (active, passive) path -> TypingLTS.Moves.move * TypingLTS.position * t

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

  type (_, _) steps =
    | End : ('s, 's) steps
    | Opponent_step :
        Moves.move * TypingLTS.position * (active, 'e) steps
        -> (passive, 'e) steps
    | Player_step :
        Moves.move * TypingLTS.position * (passive, 'e) steps
        -> (active, 'e) steps

  type ('s, 'e) path = { initial: TypingLTS.position; steps: ('s, 'e) steps }
  type t = (passive, passive) path
  type 's any_end = Any_end : ('s, 'e) path * 'e status -> 's any_end
  type 's any_steps = Any_steps : ('s, 'e) steps * 'e status -> 's any_steps

  let invalid_step position move =
    Util.Error.failwithf "Play: the move %a is not a valid step at %a"
      Moves.pp_move move TypingLTS.pp_position position

  let step position action =
    match TypingLTS.check_move position action with
    | Some target -> target
    | None -> invalid_step position (snd action)

  let rec steps_of_actions : type s.
      s status -> TypingLTS.position -> Moves.pol_move list -> s any_steps =
   fun status position actions ->
    match (status, actions) with
    | (_, []) -> Any_steps (End, status)
    | (Passive, ((Moves.Input, move) as action) :: actions) ->
        let target = step position action in
        let (Any_steps (steps, ending)) =
          steps_of_actions Active target actions in
        Any_steps (Opponent_step (move, target, steps), ending)
    | (Active, ((Moves.Output, move) as action) :: actions) ->
        let target = step position action in
        let (Any_steps (steps, ending)) =
          steps_of_actions Passive target actions in
        Any_steps (Player_step (move, target, steps), ending)
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
    | Opponent_step (_, target, steps) -> final_position_of_steps target steps
    | Player_step (_, target, steps) -> final_position_of_steps target steps

  let final_position path = final_position_of_steps path.initial path.steps

  let rec actions_of_steps : type s e. (s, e) steps -> Moves.pol_move list =
    function
    | End -> []
    | Opponent_step (move, _, steps) ->
        (Moves.Input, move) :: actions_of_steps steps
    | Player_step (move, _, steps) ->
        (Moves.Output, move) :: actions_of_steps steps

  let actions path = actions_of_steps path.steps

  let opponent_step (play : t) =
    match play.steps with
    | End -> None
    | Opponent_step (move, target, steps) ->
        Some (move, target, { initial= target; steps })

  let player_step (path : (active, passive) path) =
    match path.steps with
    | Player_step (move, target, steps) ->
        (move, target, { initial= target; steps })

  let rec extend_steps : type s.
      TypingLTS.position ->
      (s, active) steps ->
      Moves.move ->
      (s, passive) steps =
   fun position steps move ->
    match steps with
    | End -> Player_step (move, step position (Moves.Output, move), End)
    | Opponent_step (opponent_move, target, steps) ->
        Opponent_step (opponent_move, target, extend_steps target steps move)
    | Player_step (player_move, target, steps) ->
        Player_step (player_move, target, extend_steps target steps move)

  let extend_by_player_move path move =
    { path with steps= extend_steps path.initial path.steps move }

  let rec drop_last_step : type s.
      (s, active) steps -> (s, passive) steps option = function
    | End -> None
    | Opponent_step (_, _, End) -> Some End
    | Opponent_step (opponent_move, target, steps) ->
        Option.map
          (fun steps -> Opponent_step (opponent_move, target, steps))
          (drop_last_step steps)
    | Player_step (player_move, target, steps) ->
        Option.map
          (fun steps -> Player_step (player_move, target, steps))
          (drop_last_step steps)

  let drop_last_move path =
    Option.map (fun steps -> { path with steps }) (drop_last_step path.steps)

  (* The inclusion maps this path's Player names to the other participant's
     Opponent names, extended by the moves placed there. *)
  let rec dual_steps : type a b c d.
      TypingLTS.position ->
      (Namectx.Names.name, Namectx.Names.name) Util.Pmap.pmap ->
      (a * b, c * d) steps ->
      (b * a, d * c) steps option =
   fun position inclusion steps ->
    let open Util.Monad.Option in
    match steps with
    | End -> return End
    | Opponent_step (move, _, steps) ->
        let move =
          Moves.map_free_names
            (fun name -> Util.Pmap.lookup_exn name inclusion)
            move in
        let* target = TypingLTS.check_move position (Moves.Output, move) in
        let* steps = dual_steps target inclusion steps in
        return (Player_step (move, target, steps))
    | Player_step (move, _, steps) ->
        let placed = TypingLTS.weaken_move position Moves.Input move in
        let* target = TypingLTS.check_move position (Moves.Input, placed) in
        let inclusion =
          Util.Pmap.concat inclusion
            (Util.Pmap.list_to_pmap
               (List.combine
                  (Moves.get_fresh_names move)
                  (Moves.get_fresh_names placed))) in
        let* steps = dual_steps target inclusion steps in
        return (Opponent_step (placed, target, steps))

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
