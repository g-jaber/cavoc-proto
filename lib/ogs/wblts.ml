module Make (Moves : Lts.Moves.POLMOVES) :
  Lts.Hislts.HISLTS_INIT
    with type move = Moves.pol_move
     and type name = Moves.Renaming.Namectx.Names.name = struct
  type move = Moves.pol_move
  type status = Active | Passive

  (*let string_of_status = function | Active -> "active" | Passive -> "passive"*)
  type conf = status * Moves.Renaming.Namectx.Names.name list

  let conf_to_yojson (_, cstack) =
    `List
      (List.map
         (fun x -> `String (Moves.Renaming.Namectx.Names.string_of_name x))
         cstack)

  let pp_conf fmt (_, stack) =
    match stack with
    | [] -> Format.fprintf fmt "Stack: ⋅"
    | cstack ->
        let pp_sep fmt () = Format.fprintf fmt "::" in
        let pp_stack =
          Format.pp_print_list ~pp_sep Moves.Renaming.Namectx.Names.pp_name in
        Format.fprintf fmt "Stack: %a" pp_stack cstack

  let string_of_conf = Format.asprintf "%a" pp_conf

  let trans_check (status, cstack) (dir, move) =
    match (status, dir) with
    | (Active, Moves.Output) ->
        let support = Moves.Renaming.Namectx.get_names (Moves.get_namectx move) in
        let cstack' = List.filter Moves.Renaming.Namectx.Names.is_cname support in
        Some (Passive, cstack' @ cstack)
    | (Passive, Moves.Input) ->
        let subject_name = Moves.get_subject_name move in
        begin
          match (subject_name, cstack) with
          | (Some nn, cn :: cstack') when nn = cn ->
              Some (Active, cstack')
              (*We pop the continuation name *)
          | (Some nn, _) when not @@ Moves.Renaming.Namectx.Names.is_cname nn ->
              Some (Active, cstack)
          | (_, _) -> None
        end
    | _ -> None

  type name = Moves.Renaming.Namectx.Names.name

  let init_act_conf _ _ = (Active, [])
  let init_pas_conf _ _ = (Passive, [])
end
