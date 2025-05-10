module Make
    (ContNames : Lang.Names.CONT_NAMES)
    (Moves : Lts.Moves.MOVES with type name = ContNames.name) :
  Lts.Hislts.HISLTS_INIT
    with type move = Moves.move
     and type name = ContNames.name = struct
  type move = Moves.move
  type active_conf = ContNames.cont_name list
  type passive_conf = ContNames.cont_name list

  let passive_conf_to_yojson cn_l =
    `List (List.map (fun x -> `String (ContNames.string_of_cont_name x)) cn_l)

  let pp_active_conf fmt = function
    | [] -> Format.fprintf fmt "Stack: ⋅"
    | cstack ->
        let pp_sep fmt () = Format.fprintf fmt "::" in
        let pp_stack = Format.pp_print_list ~pp_sep ContNames.pp_cont_name in
        Format.fprintf fmt "Stack: %a" pp_stack cstack

  let pp_passive_conf = pp_active_conf
  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let p_trans cstack move =
    let support = Moves.get_transmitted_names move in
    let cstack' = List.filter_map ContNames.get_cont_name support in
    cstack' @ cstack

  let o_trans_check cstack move =
    let subject_names = Moves.get_subject_name move in
    let subject_cnames = List.filter_map ContNames.get_cont_name subject_names in
    match (subject_cnames, cstack) with
    | ([ cn ], cn' :: cstack') when cn = cn' ->
        Some cstack' (*We only deal with popping a single continuation name *)
    | ([], _) -> Some cstack
    | (_, _) -> None

  type name = ContNames.name

  let init_aconf _ = []
  let init_pconf _ _ = []
end
