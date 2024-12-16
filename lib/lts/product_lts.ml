module Make
    (IntLts : Bipartite.INT_LTS)
    (HistLts :
      Hislts.HISLTS_INIT
        with type move = IntLts.Int.Actions.Moves.move
         and type name = IntLts.Int.Name.name) :
  Bipartite.INT_LTS with module Int = IntLts.Int = struct
  module M = IntLts.M
  open M

  (* *)
  module Int = IntLts.Int
  module Actions = IntLts.Actions

  type active_conf = IntLts.active_conf * HistLts.active_conf

  type passive_conf = IntLts.passive_conf * HistLts.passive_conf
  [@@deriving to_yojson]

  type conf = Active of active_conf | Passive of passive_conf

  let pp_active_conf fmt (iconf, hconf) =
    Format.fprintf fmt "@[%a |@, %a@]" IntLts.pp_active_conf iconf
      HistLts.pp_active_conf hconf

  let pp_passive_conf fmt (iconf, hconf) =
    Format.fprintf fmt "@[%a |@, %a@]" IntLts.pp_passive_conf iconf
      HistLts.pp_passive_conf hconf

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let extract_interactive_ctx = function
    | Active (a_iconf, _) -> IntLts.extract_interactive_ctx (Active a_iconf)
    | Passive (p_iconf, _) -> IntLts.extract_interactive_ctx (Passive p_iconf)

  let equiv_act_conf _ _ = failwith "Not yet implemented"

  let p_trans (active_iconf, active_hconf) =
    let (action, passive_iconf_option) = IntLts.p_trans active_iconf in
    match
      (IntLts.Actions.get_move_from_action action, passive_iconf_option)
    with
    | (Some output_move, Some passive_iconf) ->
        let passive_hconf = HistLts.p_trans active_hconf output_move in
        (action, Some (passive_iconf, passive_hconf))
    | (None, None) -> (action, None)
    | _ -> failwith "Impossible branch"

  let o_trans (passive_iconf, passive_hconf) input_move =
    match
      ( IntLts.o_trans passive_iconf input_move,
        HistLts.o_trans_check passive_hconf input_move )
    with
    | (None, _) | (_, None) -> None
    | (Some active_iconf, Some active_hconf) -> Some (active_iconf, active_hconf)

  let o_trans_gen (passive_iconf, passive_hconf) =
    let* (input_move, active_iconf) = IntLts.o_trans_gen passive_iconf in
    match HistLts.o_trans_check passive_hconf input_move with
    | None -> fail ()
    | Some active_hconf -> return (input_move, (active_iconf, active_hconf))

  let init_aconf comp namectxP =
    let init_iconf = IntLts.init_aconf comp namectxP in
    let init_hconf =
      HistLts.init_aconf (IntLts.Int.IntLang.get_names_from_name_ctx namectxP)
    in
    (init_iconf, init_hconf)

  let init_pconf store ienv namectxP namectxO =
    let init_iconf = IntLts.init_pconf store ienv namectxP namectxO in
    let init_hconf =
      HistLts.init_pconf
        (IntLts.Int.IntLang.get_names_from_name_ctx namectxP)
        (IntLts.Int.IntLang.get_names_from_name_ctx namectxO) in
    (init_iconf, init_hconf)
end
