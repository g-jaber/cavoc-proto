module MakeNameIndexed (TypingLTS : Lts.Typing.LTS) :
  Lts.Typing.LTS
    with module Moves = TypingLTS.Moves
     and type store_ctx = TypingLTS.store_ctx = struct
  module Moves = TypingLTS.Moves
  module BranchMonad = TypingLTS.BranchMonad
  module View = Lts.View.Make (Moves.Thinning)

  type store_ctx = TypingLTS.store_ctx
  type active_conf = View.view_map [@@deriving to_yojson]
  type passive_conf = View.t * View.view_map [@@deriving to_yojson]

  let pp_active_conf fmt view_map =
    Format.fprintf fmt "View map: %a" View.pp_view_map view_map

  let pp_passive_conf fmt (view, view_map) =
    Format.fprintf fmt "@[⟨View: %a |@, View map: %a⟩@]" Moves.Thinning.pp view
      View.pp_view_map view_map

  type conf = Active of active_conf | Passive of passive_conf
  [@@deriving to_yojson]

  let pp_conf fmt = function
    | Active active_conf -> pp_active_conf fmt active_conf
    | Passive passive_conf -> pp_passive_conf fmt passive_conf

  let init_view_map view namectxO =
    View.init_view_map view (Moves.Renaming.Namectx.get_names namectxO)

  let init_active_conf namectxP namectxO =
    let view = Moves.Thinning.id namectxP in
    Active (init_view_map view namectxO)

  let init_passive_conf namectxP namectxO =
    let view = Moves.Thinning.id namectxP in
    Passive (view, init_view_map view namectxO)

  let check_visibility ~target_namectxP conf (direction, move) =
    match (conf, direction) with
    | (Active view_map, Moves.Output) ->
        let view =
          View.restore_view_at_subject view_map
            (Moves.get_subject_name move)
            ~context:target_namectxP
            ~fresh:(Moves.get_fresh_names move) in
        Some (Passive (view, view_map))
    | (Passive (view, view_map), Moves.Input) ->
        let view = View.transport_to_context ~context:target_namectxP view in
        if View.contains view (Moves.get_subject_name move) then
          Some
            (Active
               (View.record_view_at_introduction view_map view
                  (Moves.get_fresh_names move)))
        else None
    | _ -> None

  type position = TypingLTS.position * conf [@@deriving to_yojson]

  let pp_position fmt (position, conf) =
    Format.fprintf fmt "@[%a |@, %a@]" TypingLTS.pp_position position pp_conf
      conf

  let string_of_position = Format.asprintf "%a" pp_position
  let get_namectxO (position, _) = TypingLTS.get_namectxO position
  let get_namectxP (position, _) = TypingLTS.get_namectxP position
  let get_storectx (position, _) = TypingLTS.get_storectx position

  let place (position, _) direction subject local_namectx =
    TypingLTS.place position direction subject local_namectx

  let generate_moves (position, conf) =
    let open BranchMonad in
    let* (move, position') = TypingLTS.generate_moves position in
    let target_namectxP = TypingLTS.get_namectxP position' in
    match check_visibility ~target_namectxP conf move with
    | None -> fail ()
    | Some conf' -> return (move, (position', conf'))

  let check_move (position, conf) move =
    match TypingLTS.check_move position move with
    | None -> None
    | Some position' ->
        let target_namectxP = TypingLTS.get_namectxP position' in
        Option.map
          (fun conf' -> (position', conf'))
          (check_visibility ~target_namectxP conf move)

  let trigger_move (position, conf) move =
    let position' = TypingLTS.trigger_move position move in
    let target_namectxP = TypingLTS.get_namectxP position' in
    match check_visibility ~target_namectxP conf move with
    | Some conf' -> (position', conf')
    | None ->
        failwith "Trying to trigger a move rejected by name-indexed visibility."

  let init_act_pos store_ctx namectxP namectxO =
    let position = TypingLTS.init_act_pos store_ctx namectxP namectxO in
    let namectxP = TypingLTS.get_namectxP position in
    let namectxO = TypingLTS.get_namectxO position in
    (position, init_active_conf namectxP namectxO)

  let init_pas_pos store_ctx namectxP namectxO =
    let position = TypingLTS.init_pas_pos store_ctx namectxP namectxO in
    let namectxP = TypingLTS.get_namectxP position in
    let namectxO = TypingLTS.get_namectxO position in
    (position, init_passive_conf namectxP namectxO)
end

(* In direct style, questions push the current view on a stack and answers pop it. *)
module MakeStackBased (TypingLTS : Lts.Typing.LTS) :
  Lts.Typing.LTS
    with module Moves = TypingLTS.Moves
     and type store_ctx = TypingLTS.store_ctx = struct
  module Moves = TypingLTS.Moves
  module BranchMonad = TypingLTS.BranchMonad
  module View = Lts.View.Make (Moves.Thinning)

  type store_ctx = TypingLTS.store_ctx

  let pp_view_stack fmt = function
    | [] -> Format.pp_print_string fmt "⋅"
    | stack ->
        let pp_sep fmt () = Format.pp_print_string fmt "::" in
        Format.pp_print_list ~pp_sep Moves.Thinning.pp fmt stack

  type active_conf = View.view_map * View.t list [@@deriving to_yojson]

  type passive_conf = View.t * View.view_map * View.t list
  [@@deriving to_yojson]

  let pp_active_conf fmt (view_map, stack) =
    Format.fprintf fmt "@[⟨View map: %a |@, View stack: %a⟩@]" View.pp_view_map
      view_map pp_view_stack stack

  let pp_passive_conf fmt (view, view_map, stack) =
    Format.fprintf fmt "@[⟨View: %a |@, View map: %a |@, View stack: %a⟩@]"
      Moves.Thinning.pp view View.pp_view_map view_map pp_view_stack stack

  type conf = Active of active_conf | Passive of passive_conf
  [@@deriving to_yojson]

  let pp_conf fmt = function
    | Active active_conf -> pp_active_conf fmt active_conf
    | Passive passive_conf -> pp_passive_conf fmt passive_conf

  let non_cnames =
    List.filter (fun name -> not (Moves.Renaming.Namectx.Names.is_cname name))

  let initial_view namectxP =
    Moves.Thinning.of_support namectxP
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

  let check_visibility ~target_namectxP conf (direction, move) =
    match (conf, direction) with
    | (Active (view_map, stack), Moves.Output) ->
        let subject = Moves.get_subject_name move in
        let fresh = non_cnames (Moves.get_fresh_names move) in
        if Moves.Renaming.Namectx.Names.is_cname subject then begin
          match stack with
          | saved_view :: stack' ->
              let view =
                View.restore_view saved_view ~context:target_namectxP ~fresh
              in
              Some (Passive (view, view_map, stack'))
          | [] ->
              Util.Error.failwithf
                "Error: Proponent answers while no Opponent question is \
                 pending. Please report."
        end
        else
          let view =
            View.restore_view_at_subject view_map subject
              ~context:target_namectxP ~fresh in
          Some (Passive (view, view_map, stack))
    | (Passive (view, view_map, stack), Moves.Input) ->
        let view = View.transport_to_context ~context:target_namectxP view in
        let subject = Moves.get_subject_name move in
        let view_map' =
          View.record_view_at_introduction view_map view
            (non_cnames (Moves.get_fresh_names move)) in
        if Moves.Renaming.Namectx.Names.is_cname subject then
          Some (Active (view_map', stack))
        else if View.contains view subject then
          Some (Active (view_map', view :: stack))
        else None
    | _ -> None

  type position = TypingLTS.position * conf [@@deriving to_yojson]

  let pp_position fmt (position, conf) =
    Format.fprintf fmt "@[%a |@, %a@]" TypingLTS.pp_position position pp_conf
      conf

  let string_of_position = Format.asprintf "%a" pp_position
  let get_namectxO (position, _) = TypingLTS.get_namectxO position
  let get_namectxP (position, _) = TypingLTS.get_namectxP position
  let get_storectx (position, _) = TypingLTS.get_storectx position

  let place (position, _) direction subject local_namectx =
    TypingLTS.place position direction subject local_namectx

  let generate_moves (position, conf) =
    let open BranchMonad in
    let* (move, position') = TypingLTS.generate_moves position in
    let target_namectxP = TypingLTS.get_namectxP position' in
    match check_visibility ~target_namectxP conf move with
    | None -> fail ()
    | Some conf' -> return (move, (position', conf'))

  let check_move (position, conf) move =
    match TypingLTS.check_move position move with
    | None -> None
    | Some position' ->
        let target_namectxP = TypingLTS.get_namectxP position' in
        Option.map
          (fun conf' -> (position', conf'))
          (check_visibility ~target_namectxP conf move)

  let trigger_move (position, conf) move =
    let position' = TypingLTS.trigger_move position move in
    let target_namectxP = TypingLTS.get_namectxP position' in
    match check_visibility ~target_namectxP conf move with
    | Some conf' -> (position', conf')
    | None ->
        failwith "Trying to trigger a move rejected by stack-based visibility."

  let init_act_pos store_ctx namectxP namectxO =
    let position = TypingLTS.init_act_pos store_ctx namectxP namectxO in
    let namectxP = TypingLTS.get_namectxP position in
    let namectxO = TypingLTS.get_namectxO position in
    (position, init_active_conf namectxP namectxO)

  let init_pas_pos store_ctx namectxP namectxO =
    let position = TypingLTS.init_pas_pos store_ctx namectxP namectxO in
    let namectxP = TypingLTS.get_namectxP position in
    let namectxO = TypingLTS.get_namectxO position in
    (position, init_passive_conf namectxP namectxO)
end
