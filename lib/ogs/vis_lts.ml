module Make
    (ContNames : Lang.Names.CONT_NAMES)
    (Moves : Lts.Moves.MOVES with type name = ContNames.name) :
  Lts.Hislts.HISLTS_INIT
    with type move = Moves.move
     and type name = ContNames.name = struct
  (* the view contains the names that Proponent has provided to Opponent. *)
  type view = Moves.name list

  let view_to_yojson nn_l =
    `List (List.map (fun x -> `String (ContNames.string_of_name x)) nn_l)

  (* the view map associate to each Opponent name the set of Proponent names
     available when it was introduced. *)
  type view_map = (Moves.name, view) Util.Pmap.pmap

  let view_map_to_yojson vmap =
    let to_string (nn, view) =
      (ContNames.string_of_name nn, view_to_yojson view) in
    `Assoc (Util.Pmap.to_list @@ Util.Pmap.map to_string vmap)

  let pp_view fmt v =
    let pp_sep fmt () = Format.pp_print_char fmt ',' in
    let pp_view_aux = Format.pp_print_list ~pp_sep ContNames.pp_name in
    Format.fprintf fmt "{%a}" pp_view_aux v

  let pp_view_map fmt vm =
    let pp_empty fmt () = Format.pp_print_char fmt '.' in
    let pp_pair fmt (n, v) =
      Format.fprintf fmt "%a ↦ %a" ContNames.pp_name n pp_view v in
    Util.Pmap.pp_pmap ~pp_empty pp_pair fmt vm

  type move = Moves.move
  type active_conf = view_map
  type passive_conf = view * view_map [@@deriving to_yojson]

  let pp_active_conf fmt vm = Format.fprintf fmt "View map: %a" pp_view_map vm

  let pp_passive_conf fmt (v, vm) =
    Format.fprintf fmt "@[⟨View: %a |@, View map: %a⟩@]" pp_view v pp_view_map
      vm

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let get_subject_name move =
    match Moves.get_subject_name move with
    | [ nn ] -> nn
    | [] ->
        Util.Error.failwithf
          "Error: the move %a does not have a subject name. We cannot enforce \
           visibility on it."
          Moves.pp_move move
    | _ ->
        Util.Error.failwithf
          "Error: the move %a  has multiple subject names. We cannot enforce \
           visibility on it."
          Moves.pp_move move

  let p_trans vm move =
    let nn = get_subject_name move in
    let view =
      match Util.Pmap.lookup nn vm with
      | Some view -> Moves.get_transmitted_names move @ view
      | None ->
          Util.Error.failwithf
            "Error: the name %a is not in the view map %a. Please report."
            ContNames.pp_name nn pp_view_map vm in
    (view, vm)

  let o_trans_check (view, vm) move =
    let nn = get_subject_name move in
    if List.mem nn view then
      let freshn_l = Moves.get_transmitted_names move in
      let vm_l = List.map (fun nn -> (nn, view)) freshn_l in
      let vm' = Util.Pmap.list_to_pmap vm_l in
      Some (Util.Pmap.concat vm vm')
    else None

  type name = ContNames.name

  let init_aconf _ = Util.Pmap.empty
  let init_pconf nameP _ = (nameP, Util.Pmap.empty)
end
