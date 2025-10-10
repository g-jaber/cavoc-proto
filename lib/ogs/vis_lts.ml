module Make (Moves : Lts.Moves.POLMOVES) :
  Lts.Hislts.HISLTS_INIT
    with type move = Moves.pol_move
     and type name = Moves.Renaming.Namectx.Names.name = struct
  (* the view contains the names that Proponent has provided to Opponent. *)
  type view = Moves.Renaming.Namectx.Names.name list

  let view_to_yojson nn_l =
    `List
      (List.map (fun x -> `String (Moves.Renaming.Namectx.Names.string_of_name x)) nn_l)

  (* the view map associate to each Opponent name the set of Proponent names
     available when it was introduced. *)
  type view_map = (Moves.Renaming.Namectx.Names.name, view) Util.Pmap.pmap

  let view_map_to_yojson vmap =
    let to_string (nn, view) =
      (Moves.Renaming.Namectx.Names.string_of_name nn, view_to_yojson view) in
    `Assoc (Util.Pmap.to_list @@ Util.Pmap.map to_string vmap)

  let pp_view fmt v =
    let pp_sep fmt () = Format.pp_print_char fmt ',' in
    let pp_view_aux = Format.pp_print_list ~pp_sep Moves.Renaming.Namectx.Names.pp_name in
    Format.fprintf fmt "{%a}" pp_view_aux v

  let pp_view_map fmt vm =
    let pp_empty fmt () = Format.pp_print_char fmt '.' in
    let pp_pair fmt (n, v) =
      Format.fprintf fmt "%a ↦ %a" Moves.Renaming.Namectx.Names.pp_name n pp_view v in
    Util.Pmap.pp_pmap ~pp_empty pp_pair fmt vm

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
        let nn =  Moves.get_subject_name move in
        let view =
          match Util.Pmap.lookup nn vm with
          | Some view -> Moves.Renaming.Namectx.get_names (Moves.get_namectx move) @ view
          | None ->
              Util.Error.failwithf
                "Error: the name %a is not in the view map %a. Please report."
                Moves.Renaming.Namectx.Names.pp_name nn pp_view_map vm in
        Some (Passive (view, vm))
    | (Passive (view, vm), Moves.Input) ->
        let nn =  Moves.get_subject_name move in
        if List.mem nn view then
          let freshn_l = Moves.Renaming.Namectx.get_names (Moves.get_namectx move) in
          let vm_l = List.map (fun nn -> (nn, view)) freshn_l in
          let vm' = Util.Pmap.list_to_pmap vm_l in
          Some (Active (Util.Pmap.concat vm vm'))
        else None
    | _ -> None

  type name = Moves.Renaming.Namectx.Names.name

  let init_act_conf _ _ = Active Util.Pmap.empty
  let init_pas_conf nameP _ = Passive (nameP, Util.Pmap.empty)
end
