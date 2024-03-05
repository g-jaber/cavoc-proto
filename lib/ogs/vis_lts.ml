module Make
    (ContNames : Lang.Names.CONT_NAMES)
    (Moves : Lts.Moves.MOVES with type name = ContNames.name) :
  Lts.Hislts.HISLTS_INIT
    with type move = Moves.move
     and type name = ContNames.name = struct
  (* the view contains the names that Proponent has provided to Opponent. *)
  type view = Moves.name list

  (* the view map associate to each Opponent name the set of Proponent names
     available when it was introduced. *)
  type view_map = (Moves.name, view) Util.Pmap.pmap

  let string_of_view v =
    let str_l = List.map ContNames.string_of_name v in
    String.concat "," str_l

  let string_of_view_map =
    Util.Pmap.string_of_pmap "[]" "->" ContNames.string_of_name string_of_view

  type move = Moves.move
  type active_conf = view_map
  type passive_conf = view * view_map

  let string_of_active_conf = string_of_view_map

  let string_of_passive_conf (v, vm) =
    "<" ^ string_of_view v ^ "|" ^ string_of_view_map vm ^ ">"

  let get_subject_name move =
    match Moves.get_subject_name move with
    | [ nn ] -> nn
    | [] ->
        failwith @@ "Error: the move " ^ Moves.string_of_move move
        ^ " does not have a subject name. We cannot enforce \n\
          \      visibility on it."
    | _ ->
        failwith @@ "Error: the move " ^ Moves.string_of_move move
        ^ " has multiple subject names. We cannot enforce \n\
          \      visibility on it."

  let p_trans vm move =
    let nn = get_subject_name move in
    let view =
      match Util.Pmap.lookup nn vm with
      | Some view -> (Moves.get_transmitted_names move)@view
      | None ->
          failwith @@ "Error: the name "
          ^ ContNames.string_of_name nn
          ^ " is not in the view map " ^ string_of_view_map vm
          ^ ". Please report." in
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
