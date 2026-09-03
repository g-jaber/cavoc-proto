(* The visibility discipline over a typing LTS. *)
module type VISIBILITY = sig
  module TypingLTS : Lts.Typing.LTS

  type conf [@@deriving to_yojson]

  val pp_conf : Format.formatter -> conf -> unit

  val init_active_conf :
    TypingLTS.Moves.Renaming.Namectx.t ->
    TypingLTS.Moves.Renaming.Namectx.t ->
    conf

  val init_passive_conf :
    TypingLTS.Moves.Renaming.Namectx.t ->
    TypingLTS.Moves.Renaming.Namectx.t ->
    conf

  (* From the Player context after the move and the weakening of the
     local context of the move. *)
  val check_visibility :
    TypingLTS.Moves.Renaming.Namectx.t ->
    TypingLTS.Moves.Renaming.t ->
    conf ->
    TypingLTS.Moves.pol_move ->
    conf option
end

module Make (Visibility : VISIBILITY) :
  Lts.Typing.LTS
    with module Moves = Visibility.TypingLTS.Moves
     and module BranchMonad = Visibility.TypingLTS.BranchMonad
     and type store_ctx = Visibility.TypingLTS.store_ctx = struct
  module TypingLTS = Visibility.TypingLTS
  module Moves = TypingLTS.Moves
  module BranchMonad = TypingLTS.BranchMonad

  type store_ctx = TypingLTS.store_ctx
  type position = TypingLTS.position * Visibility.conf [@@deriving to_yojson]

  let pp_position fmt (position, conf) =
    Format.fprintf fmt "@[%a |@, %a@]" TypingLTS.pp_position position
      Visibility.pp_conf conf

  let string_of_position = Format.asprintf "%a" pp_position
  let get_namectxO (position, _) = TypingLTS.get_namectxO position
  let get_namectxP (position, _) = TypingLTS.get_namectxP position
  let get_storectx (position, _) = TypingLTS.get_storectx position

  let generate_moves (position, conf) =
    let open BranchMonad in
    let* (move, weakening, position') = TypingLTS.generate_moves position in
    match
      Visibility.check_visibility
        (TypingLTS.get_namectxP position')
        weakening conf move
    with
    | None -> fail ()
    | Some conf' -> return (move, weakening, (position', conf'))

  let check_move (position, conf) move =
    match TypingLTS.check_move position move with
    | None -> None
    | Some (weakening, position') ->
        Option.map
          (fun conf' -> (weakening, (position', conf')))
          (Visibility.check_visibility
             (TypingLTS.get_namectxP position')
             weakening conf move)

  let trigger_move (position, conf) move =
    let (weakening, position') = TypingLTS.trigger_move position move in
    match
      Visibility.check_visibility
        (TypingLTS.get_namectxP position')
        weakening conf move
    with
    | Some conf' -> (weakening, (position', conf'))
    | None -> failwith "Trying to trigger a move rejected by visibility."

  let init_act_pos store_ctx namectxP namectxO =
    let position = TypingLTS.init_act_pos store_ctx namectxP namectxO in
    ( position,
      Visibility.init_active_conf
        (TypingLTS.get_namectxP position)
        (TypingLTS.get_namectxO position) )

  let init_pas_pos store_ctx namectxP namectxO =
    let position = TypingLTS.init_pas_pos store_ctx namectxP namectxO in
    ( position,
      Visibility.init_passive_conf
        (TypingLTS.get_namectxP position)
        (TypingLTS.get_namectxO position) )
end

(* Name-indexed visibility: each Opponent name records the Player view
   current when it was introduced. *)
module NameIndexed (TypingLTS : Lts.Typing.LTS) :
  VISIBILITY with module TypingLTS = TypingLTS = struct
  module TypingLTS = TypingLTS
  module Moves = TypingLTS.Moves
  module View = Lts.View.Make (Moves.Renaming)

  type active_conf = View.view_map [@@deriving to_yojson]
  type passive_conf = View.t * View.view_map [@@deriving to_yojson]

  type conf = Active of active_conf | Passive of passive_conf
  [@@deriving to_yojson]

  let pp_conf fmt = function
    | Active view_map ->
        Format.fprintf fmt "View map: %a" View.pp_view_map view_map
    | Passive (view, view_map) ->
        Format.fprintf fmt "@[⟨View: %a |@, View map: %a⟩@]" Moves.Renaming.pp
          view View.pp_view_map view_map

  let init_view_map view namectxO =
    View.init_view_map view (Moves.Renaming.Namectx.get_names namectxO)

  let init_active_conf namectxP namectxO =
    let view = Moves.Renaming.id namectxP in
    Active (init_view_map view namectxO)

  let init_passive_conf namectxP namectxO =
    let view = Moves.Renaming.id namectxP in
    Passive (view, init_view_map view namectxO)

  let check_visibility target_namectxP weakening conf (direction, move) =
    match (conf, direction) with
    | (Active view_map, Moves.Output) ->
        let view =
          View.restore_view_at_subject view_map
            (Moves.get_subject_name move)
            target_namectxP
            (Moves.fresh_names weakening move) in
        Some (Passive (view, view_map))
    | (Passive (view, view_map), Moves.Input) ->
        let view = View.transport_to_context view target_namectxP in
        if View.contains view (Moves.get_subject_name move) then
          Some
            (Active
               (View.record_view_at_introduction view_map view
                  (Moves.fresh_names weakening move)))
        else None
    | _ -> None
end

module MakeNameIndexed (TypingLTS : Lts.Typing.LTS) =
  Make (NameIndexed (TypingLTS))

(* Stack-based visibility, for direct style: questions push the current view
   on a stack and answers pop it. *)
module StackBased (TypingLTS : Lts.Typing.LTS) :
  VISIBILITY with module TypingLTS = TypingLTS = struct
  module TypingLTS = TypingLTS
  module Moves = TypingLTS.Moves
  module View = Lts.View.Make (Moves.Renaming)

  let pp_view_stack fmt = function
    | [] -> Format.pp_print_string fmt "⋅"
    | stack ->
        let pp_sep fmt () = Format.pp_print_string fmt "::" in
        Format.pp_print_list ~pp_sep Moves.Renaming.pp fmt stack

  type active_conf = View.view_map * View.t list [@@deriving to_yojson]

  type passive_conf = View.t * View.view_map * View.t list
  [@@deriving to_yojson]

  type conf = Active of active_conf | Passive of passive_conf
  [@@deriving to_yojson]

  let pp_conf fmt = function
    | Active (view_map, stack) ->
        Format.fprintf fmt "@[⟨View map: %a |@, View stack: %a⟩@]"
          View.pp_view_map view_map pp_view_stack stack
    | Passive (view, view_map, stack) ->
        Format.fprintf fmt "@[⟨View: %a |@, View map: %a |@, View stack: %a⟩@]"
          Moves.Renaming.pp view View.pp_view_map view_map pp_view_stack stack

  let non_cnames =
    List.filter (fun name -> not (Moves.Renaming.Namectx.Names.is_cname name))

  let initial_view namectxP =
    Moves.Renaming.of_support namectxP
      (non_cnames (Moves.Renaming.Namectx.get_names namectxP))

  let init_view_map view namectxO =
    View.init_view_map view
      (non_cnames (Moves.Renaming.Namectx.get_names namectxO))

  let init_active_conf namectxP namectxO =
    let view = initial_view namectxP in
    Active (init_view_map view namectxO, [ view ])

  let init_passive_conf namectxP namectxO =
    let view = initial_view namectxP in
    Passive (view, init_view_map view namectxO, [])

  let check_visibility target_namectxP weakening conf (direction, move) =
    match (conf, direction) with
    | (Active (view_map, stack), Moves.Output) ->
        let subject = Moves.get_subject_name move in
        let fresh = non_cnames (Moves.fresh_names weakening move) in
        if Moves.Renaming.Namectx.Names.is_cname subject then begin
          match stack with
          | saved_view :: stack' ->
              let view = View.restore_view saved_view target_namectxP fresh in
              Some (Passive (view, view_map, stack'))
          | [] ->
              Util.Error.failwithf
                "Error: Proponent answers while no Opponent question is \
                 pending. Please report."
        end
        else
          let view =
            View.restore_view_at_subject view_map subject target_namectxP fresh
          in
          Some (Passive (view, view_map, stack))
    | (Passive (view, view_map, stack), Moves.Input) ->
        let view = View.transport_to_context view target_namectxP in
        let subject = Moves.get_subject_name move in
        let view_map' =
          View.record_view_at_introduction view_map view
            (non_cnames (Moves.fresh_names weakening move)) in
        if Moves.Renaming.Namectx.Names.is_cname subject then
          Some (Active (view_map', stack))
        else if View.contains view subject then
          Some (Active (view_map', view :: stack))
        else None
    | _ -> None
end

module MakeStackBased (TypingLTS : Lts.Typing.LTS) = Make (StackBased (TypingLTS))
