module Make (Int : Lts.Interactive.INT) = struct
  module M = Int.IntLang.M
  include M
  module Int = Int
  module Actions = Int.Actions

  type active_conf = {
    computation: Int.IntLang.computation;
    heap: Int.IntLang.Memory.memory;
    loc_ctx: Int.IntLang.Memory.memory_type_ctx;
    namectxO: Int.IntLang.name_ctx;
  }

  type passive_conf = {
    ienv: Int.IntLang.interactive_env;
    loc_ctx: Int.IntLang.Memory.memory_type_ctx;
    namectxO: Int.IntLang.name_ctx;
    namectxP: Int.IntLang.name_ctx;
  }

  type conf = Active of active_conf | Passive of passive_conf

  let string_of_active_conf aconf =
    "<"
    ^ Int.IntLang.string_of_computation aconf.computation
    ^ " | "
    ^ Int.IntLang.Memory.string_of_memory aconf.heap
    ^ " | "
    ^ Int.IntLang.string_of_name_ctx aconf.namectxO
    ^ ">"

  let string_of_passive_conf pconf =
    "<"
    ^ Int.IntLang.string_of_interactive_env pconf.ienv
    ^ " | "
    ^ Int.IntLang.Memory.string_of_memory_type_ctx pconf.loc_ctx
    ^ " | "
    ^ Int.IntLang.string_of_name_ctx pconf.namectxO
    ^ " | "
    ^ Int.IntLang.string_of_name_ctx pconf.namectxP
    ^ ">"

  let p_trans aconf =
    let nf_option = Int.IntLang.compute_nf (aconf.computation, aconf.heap) in
    match nf_option with
    | None -> (Int.Actions.diverging_action, None)
    | Some (kind, _) when Int.IntLang.is_error kind ->
        (Int.Actions.error_action, None)
    | Some (kind, heap) ->
        let loc_ctx = Int.IntLang.Memory.infer_type_memory heap in
        let (move, ienv, namectxP) =
          Int.generate_output_move aconf.namectxO kind in
        ( Int.Actions.inject_move move,
          Some { loc_ctx; ienv; namectxP; namectxO= aconf.namectxO } )

  let o_trans pas_conf input_move =
    match
      Int.check_input_move pas_conf.namectxP pas_conf.namectxO input_move
    with
    | None -> None
    | Some _ ->
        failwith
          "POGS o_trans cannot be implemented without moves-with-heaps !!"

  let o_trans_gen pconf =
    let* (input_move, _, namectxO) =
      Int.generate_input_moves pconf.namectxP pconf.namectxO in
    let* heap = Int.IntLang.Memory.generate_memory pconf.loc_ctx in
    let (computation, _) = Int.trigger_computation pconf.ienv input_move in
    return (input_move, { computation; heap; loc_ctx= pconf.loc_ctx; namectxO })

  let init_aconf computation namectxO =
    {
      computation;
      heap= Int.IntLang.Memory.empty_memory;
      loc_ctx= Int.IntLang.Memory.empty_memory_type_ctx;
      namectxO;
    }

  let init_pconf memory ienv namectxO namectxP =
    let memory_ctx = Int.IntLang.Memory.infer_type_memory memory in
    { loc_ctx= memory_ctx; ienv; namectxO; namectxP }

  let equiv_aconf aconf aconfb =
    aconf.computation = aconfb.computation && aconf.heap = aconfb.heap
end
