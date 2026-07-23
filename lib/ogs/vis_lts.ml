(* the view contains the names that Proponent has provided to Opponent.
   The view map associates to each Opponent name the set of Proponent names
   available when it was introduced. *)
module View (Names : Lang.Names.NAMES) = struct
  type name = Names.name
  type view = name list

  let view_to_yojson nn_l = `List (List.map Names.name_to_yojson nn_l)

  type view_map = (name, view) Util.Pmap.pmap

  let view_map_to_yojson vmap =
    let to_string (nn, view) = (Names.string_of_name nn, view_to_yojson view) in
    `Assoc (Util.Pmap.to_list @@ Util.Pmap.map to_string vmap)

  let pp_view fmt v =
    let pp_sep fmt () = Format.pp_print_char fmt ',' in
    let pp_view_aux = Format.pp_print_list ~pp_sep Names.pp_name in
    Format.fprintf fmt "{%a}" pp_view_aux v

  let pp_view_map fmt vm =
    let pp_empty fmt () = Format.pp_print_char fmt '.' in
    let pp_pair fmt (n, v) =
      Format.fprintf fmt "%a ↦ %a" Names.pp_name n pp_view v in
    Util.Pmap.pp_pmap ~pp_empty pp_pair fmt vm

  (* Initially, every Opponent name sees all the initial Proponent names. *)
  let init_view_map namesP namesO =
    Util.Pmap.list_to_pmap @@ List.map (fun nn -> (nn, namesP)) namesO
end

module Make (Moves : Lts.Moves.POLMOVES) :
  Lts.Hislts.HISLTS_INIT
    with type move = Moves.pol_move
     and type name = Moves.Renaming.Namectx.Names.name = struct
  include View (Moves.Renaming.Namectx.Names)

  type move = Moves.pol_move
  type active_conf = view_map [@@deriving to_yojson]
  type passive_conf = view * view_map [@@deriving to_yojson]

  let pp_active_conf fmt vm = Format.fprintf fmt "View map: %a" pp_view_map vm

  let pp_passive_conf fmt (v, vm) =
    Format.fprintf fmt "@[⟨View: %a |@, View map: %a⟩@]" pp_view v pp_view_map
      vm

  type conf = Active of active_conf | Passive of passive_conf
  [@@deriving to_yojson]

  let pp_conf fmt = function
    | Active aconf -> pp_active_conf fmt aconf
    | Passive pconf -> pp_passive_conf fmt pconf

  let string_of_conf = Format.asprintf "%a" pp_conf

  let trans_check conf (dir, move) =
    match (conf, dir) with
    | (Active vm, Moves.Output) ->
        let nn = Moves.get_subject_name move in
        let view =
          match Util.Pmap.lookup nn vm with
          | Some view -> view @ Moves.get_fresh_names move
          | None ->
              Util.Error.failwithf
                "Error: the name %a is not in the view map %a. Please report."
                Moves.Renaming.Namectx.Names.pp_name nn pp_view_map vm in
        Some (Passive (view, vm))
    | (Passive (view, vm), Moves.Input) ->
        let nn = Moves.get_subject_name move in
        if List.mem nn view then
          let freshn_l = Moves.get_fresh_names move in
          let vm_l = List.map (fun nn -> (nn, view)) freshn_l in
          let vm' = Util.Pmap.list_to_pmap vm_l in
          Some (Active (Util.Pmap.concat vm vm'))
        else None
    | _ -> None

  let init_act_conf namesP namesO = Active (init_view_map namesP namesO)

  let init_pas_conf namesP namesO =
    Passive (namesP, init_view_map namesP namesO)
end

(* Variant for direct style, where continuation names have no identity, so
   that the view to restore at a Proponent answer cannot be recorded in the
   view map. The interaction being necessarily well-bracketed, that view is
   instead pushed on a stack at each Opponent question and popped at each
   Proponent answer. Only function names are constrained: continuation names
   are kept out of views. *)
module MakeWb (Moves : Lts.Moves.POLMOVES) :
  Lts.Hislts.HISLTS_INIT
    with type move = Moves.pol_move
     and type name = Moves.Renaming.Namectx.Names.name = struct
  include View (Moves.Renaming.Namectx.Names)

  type move = Moves.pol_move
  type active_conf = view_map * view list [@@deriving to_yojson]
  type passive_conf = view * view_map * view list [@@deriving to_yojson]

  let pp_view_stack fmt = function
    | [] -> Format.pp_print_string fmt "⋅"
    | stack ->
        let pp_sep fmt () = Format.pp_print_string fmt "::" in
        Format.pp_print_list ~pp_sep pp_view fmt stack

  let pp_active_conf fmt (vm, stack) =
    Format.fprintf fmt "@[⟨View map: %a |@, View stack: %a⟩@]" pp_view_map vm
      pp_view_stack stack

  let pp_passive_conf fmt (v, vm, stack) =
    Format.fprintf fmt "@[⟨View: %a |@, View map: %a |@, View stack: %a⟩@]"
      pp_view v pp_view_map vm pp_view_stack stack

  type conf = Active of active_conf | Passive of passive_conf
  [@@deriving to_yojson]

  let pp_conf fmt = function
    | Active aconf -> pp_active_conf fmt aconf
    | Passive pconf -> pp_passive_conf fmt pconf

  let string_of_conf = Format.asprintf "%a" pp_conf

  let non_cnames =
    List.filter (fun nn -> not (Moves.Renaming.Namectx.Names.is_cname nn))

  let trans_check conf (dir, move) =
    match (conf, dir) with
    | (Active (vm, stack), Moves.Output) ->
        let nn = Moves.get_subject_name move in
        let freshn_l = non_cnames (Moves.get_fresh_names move) in
        if Moves.Renaming.Namectx.Names.is_cname nn then
          begin match stack with
          | view :: stack' -> Some (Passive (view @ freshn_l, vm, stack'))
          | [] ->
              Util.Error.failwithf
                "Error: Proponent answers while no Opponent question is \
                 pending. Please report."
          end
        else
          let view =
            match Util.Pmap.lookup nn vm with
            | Some view -> view @ freshn_l
            | None ->
                Util.Error.failwithf
                  "Error: the name %a is not in the view map %a. Please \
                   report."
                  Moves.Renaming.Namectx.Names.pp_name nn pp_view_map vm in
          Some (Passive (view, vm, stack))
    | (Passive (view, vm, stack), Moves.Input) ->
        let nn = Moves.get_subject_name move in
        let freshn_l = non_cnames (Moves.get_fresh_names move) in
        let vm_l = List.map (fun nn -> (nn, view)) freshn_l in
        let vm' = Util.Pmap.concat vm (Util.Pmap.list_to_pmap vm_l) in
        if Moves.Renaming.Namectx.Names.is_cname nn then
          Some (Active (vm', stack))
        else if List.mem nn view then Some (Active (vm', view :: stack))
        else None
    | _ -> None

  (* The bottom of the initial stack is the view restored by Proponent's
     toplevel answer. *)
  let init_act_conf namesP namesO =
    let namesP = non_cnames namesP in
    Active (init_view_map namesP (non_cnames namesO), [ namesP ])

  let init_pas_conf namesP namesO =
    let namesP = non_cnames namesP in
    Passive (namesP, init_view_map namesP (non_cnames namesO), [])
end
