module Make
    (Moves : Lts.Moves.POLMOVES) :
  Lts.Hislts.HISLTS_INIT
    with type move = Moves.pol_move
     and type name = Moves.Namectx.Names.name = struct
  type move = Moves.pol_move
  type status = Active | Passive

  (*let string_of_status = function | Active -> "active" | Passive -> "passive"*)
  type conf = status * Moves.Namectx.Names.name list

  let conf_to_yojson (_, cstack) =
    `List (List.map (fun x -> `String (Moves.Namectx.Names.string_of_name x)) cstack)

  let pp_conf fmt (_, stack) =
    match stack with
    | [] -> Format.fprintf fmt "Stack: ⋅"
    | cstack ->
        let pp_sep fmt () = Format.fprintf fmt "::" in
        let pp_stack = Format.pp_print_list ~pp_sep Moves.Namectx.Names.pp_name in
        Format.fprintf fmt "Stack: %a" pp_stack cstack

  let string_of_conf = Format.asprintf "%a" pp_conf

  let trans_check (status, cstack) (dir, move) =
    match (status, dir) with
    | (Active, Moves.Output) ->
        let support = Moves.get_transmitted_names move in
        let cstack' = List.filter Moves.Namectx.Names.is_cname support in
        Some (Passive, cstack' @ cstack)
    | (Passive, Moves.Input) ->
        let subject_names = Moves.get_subject_name move in
        let subject_cnames = List.filter Moves.Namectx.Names.is_cname subject_names in
        begin
          match (subject_cnames, cstack) with
          | ([ cn ], cn' :: cstack') when cn = cn' ->
              Some (Active, cstack')
              (*We only deal with popping a single continuation name *)
          | ([], _) -> Some (Active, cstack) (* Really ?*)
          | (_, _) -> None
        end
    | _ -> None

  type name = Moves.Namectx.Names.name

  let init_act_conf _ _ = (Active, [])
  let init_pas_conf _ _ = (Passive, [])
end
