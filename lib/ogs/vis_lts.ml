(* Updating a view after a move requires the target
   Proponent name context.  The two functors below therefore extend a typing
   LTS directly, rather than implementing the context-free [HISLTS]
   interface. *)

module MakeNameIndexed (TypingLTS : Lts.Typing.LTS) :
  Lts.Typing.LTS
    with module Moves = TypingLTS.Moves
     and type store_ctx = TypingLTS.store_ctx = struct
  module Moves = TypingLTS.Moves
  module BranchMonad = TypingLTS.BranchMonad
  module View = Lts.View.Make (Moves.Renaming)

  type store_ctx = TypingLTS.store_ctx
  type name = Moves.Renaming.Namectx.Names.name
  type view = View.t

  let view_to_yojson = View.to_yojson

  (* Every Opponent name remembers the Proponent view that was current when
     the name was introduced. *)
  type view_map = (name, view) Util.Pmap.pmap

  let view_map_to_yojson view_map =
    let to_entry (name, view) =
      (Moves.Renaming.Namectx.Names.string_of_name name, View.to_yojson view)
    in
    `Assoc (List.map to_entry (Util.Pmap.to_list view_map))

  let pp_view_map fmt view_map =
    let pp_empty fmt () = Format.pp_print_char fmt '.' in
    let pp_pair fmt (name, view) =
      Format.fprintf fmt "%a ↦ %a" Moves.Renaming.Namectx.Names.pp_name name
        View.pp view in
    Util.Pmap.pp_pmap ~pp_empty pp_pair fmt view_map

  type active_conf = view_map [@@deriving to_yojson]
  type passive_conf = view * view_map [@@deriving to_yojson]

  let pp_active_conf fmt view_map =
    Format.fprintf fmt "View map: %a" pp_view_map view_map

  let pp_passive_conf fmt (view, view_map) =
    Format.fprintf fmt "@[⟨View: %a |@, View map: %a⟩@]" View.pp view
      pp_view_map view_map

  type conf = Active of active_conf | Passive of passive_conf
  [@@deriving to_yojson]

  let pp_conf fmt = function
    | Active active_conf -> pp_active_conf fmt active_conf
    | Passive passive_conf -> pp_passive_conf fmt passive_conf

  let init_view_map view namectxO =
    Util.Pmap.list_to_pmap
    @@ List.map
         (fun name -> (name, view))
         (Moves.Renaming.Namectx.get_names namectxO)

  let init_active_conf namectxP namectxO =
    let view = View.full namectxP in
    Active (init_view_map view namectxO)

  let init_passive_conf namectxP namectxO =
    let view = View.full namectxP in
    Passive (view, init_view_map view namectxO)

  let check_visibility ~target_namectxP conf (direction, move) =
    match (conf, direction) with
    | (Active view_map, Moves.Output) ->
        let subject = Moves.get_subject_name move in
        let view =
          match Util.Pmap.lookup subject view_map with
          | Some saved_view ->
              saved_view
              |> View.change_context ~context:target_namectxP
              |> View.extend_visible_support ~fresh:(Moves.get_fresh_names move)
          | None ->
              Util.Error.failwithf
                "Error: the name %a is not in the view map %a. Please report."
                Moves.Renaming.Namectx.Names.pp_name subject pp_view_map
                view_map in
        Some (Passive (view, view_map))
    | (Passive (view, view_map), Moves.Input) ->
        let view = View.change_context ~context:target_namectxP view in
        let subject = Moves.get_subject_name move in
        if View.contains view subject then
          let fresh_bindings =
            List.map (fun name -> (name, view)) (Moves.get_fresh_names move)
          in
          let view_map' =
            Util.Pmap.concat view_map (Util.Pmap.list_to_pmap fresh_bindings)
          in
          Some (Active view_map')
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

(* In direct style, questions push the    current view on a stack and answers pop it. *)
module MakeStackBased (TypingLTS : Lts.Typing.LTS) :
  Lts.Typing.LTS
    with module Moves = TypingLTS.Moves
     and type store_ctx = TypingLTS.store_ctx = struct
  module Moves = TypingLTS.Moves
  module BranchMonad = TypingLTS.BranchMonad
  module View = Lts.View.Make (Moves.Renaming)

  type store_ctx = TypingLTS.store_ctx
  type name = Moves.Renaming.Namectx.Names.name
  type view = View.t

  let view_to_yojson = View.to_yojson

  type view_map = (name, view) Util.Pmap.pmap

  let view_map_to_yojson view_map =
    let to_entry (name, view) =
      (Moves.Renaming.Namectx.Names.string_of_name name, View.to_yojson view)
    in
    `Assoc (List.map to_entry (Util.Pmap.to_list view_map))

  let pp_view_map fmt view_map =
    let pp_empty fmt () = Format.pp_print_char fmt '.' in
    let pp_pair fmt (name, view) =
      Format.fprintf fmt "%a ↦ %a" Moves.Renaming.Namectx.Names.pp_name name
        View.pp view in
    Util.Pmap.pp_pmap ~pp_empty pp_pair fmt view_map

  let pp_view_stack fmt = function
    | [] -> Format.pp_print_string fmt "⋅"
    | stack ->
        let pp_sep fmt () = Format.pp_print_string fmt "::" in
        Format.pp_print_list ~pp_sep View.pp fmt stack

  type active_conf = view_map * view list [@@deriving to_yojson]
  type passive_conf = view * view_map * view list [@@deriving to_yojson]

  let pp_active_conf fmt (view_map, stack) =
    Format.fprintf fmt "@[⟨View map: %a |@, View stack: %a⟩@]" pp_view_map
      view_map pp_view_stack stack

  let pp_passive_conf fmt (view, view_map, stack) =
    Format.fprintf fmt "@[⟨View: %a |@, View map: %a |@, View stack: %a⟩@]"
      View.pp view pp_view_map view_map pp_view_stack stack

  type conf = Active of active_conf | Passive of passive_conf
  [@@deriving to_yojson]

  let pp_conf fmt = function
    | Active active_conf -> pp_active_conf fmt active_conf
    | Passive passive_conf -> pp_passive_conf fmt passive_conf

  let non_cnames =
    List.filter (fun name -> not (Moves.Renaming.Namectx.Names.is_cname name))

  let initial_view namectxP =
    View.of_support namectxP
      (non_cnames (Moves.Renaming.Namectx.get_names namectxP))

  let init_view_map view namectxO =
    Util.Pmap.list_to_pmap
    @@ List.map
         (fun name -> (name, view))
         (non_cnames (Moves.Renaming.Namectx.get_names namectxO))

  let init_active_conf namectxP namectxO =
    let view = initial_view namectxP in
    Active (init_view_map view namectxO, [ view ])

  let init_passive_conf namectxP namectxO =
    let view = initial_view namectxP in
    Passive (view, init_view_map view namectxO, [])

  let extend_view ~target_namectxP move saved_view =
    saved_view
    |> View.change_context ~context:target_namectxP
    |> View.extend_visible_support
         ~fresh:(non_cnames (Moves.get_fresh_names move))

  let check_visibility ~target_namectxP conf (direction, move) =
    match (conf, direction) with
    | (Active (view_map, stack), Moves.Output) ->
        let subject = Moves.get_subject_name move in
        if Moves.Renaming.Namectx.Names.is_cname subject then begin
          match stack with
          | saved_view :: stack' ->
              let view = extend_view ~target_namectxP move saved_view in
              Some (Passive (view, view_map, stack'))
          | [] ->
              Util.Error.failwithf
                "Error: Proponent answers while no Opponent question is \
                 pending. Please report."
        end
        else
          let view =
            match Util.Pmap.lookup subject view_map with
            | Some saved_view -> extend_view ~target_namectxP move saved_view
            | None ->
                Util.Error.failwithf
                  "Error: the name %a is not in the view map %a. Please report."
                  Moves.Renaming.Namectx.Names.pp_name subject pp_view_map
                  view_map in
          Some (Passive (view, view_map, stack))
    | (Passive (view, view_map, stack), Moves.Input) ->
        let view = View.change_context ~context:target_namectxP view in
        let subject = Moves.get_subject_name move in
        let fresh_bindings =
          List.map
            (fun name -> (name, view))
            (non_cnames (Moves.get_fresh_names move)) in
        let view_map' =
          Util.Pmap.concat view_map (Util.Pmap.list_to_pmap fresh_bindings)
        in
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
