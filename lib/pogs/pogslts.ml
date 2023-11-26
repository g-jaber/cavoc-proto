module Make (Int : Lts.Interactive.INT) = struct
  module M = Int.IntLang.M
  include M
  module Int = Int
  module Actions = Int.Actions

  type active_conf = {
    computation: Int.IntLang.computation;
    memory: Int.IntLang.Memory.memory;
    ictx: Int.interactive_ctx;
  }

  type passive_conf = {
    ienv: Int.IntLang.interactive_env;
    memory: Int.IntLang.Memory.memory;
    ictx: Int.interactive_ctx;
  }

  type conf = Active of active_conf | Passive of passive_conf

  let string_of_active_conf act_conf =
    "<"
    ^ Int.IntLang.string_of_computation act_conf.computation
    ^ " | "
    ^ Int.IntLang.Memory.string_of_memory act_conf.memory
    ^ " > | "
    ^ Int.string_of_interactive_ctx act_conf.ictx

  let string_of_passive_conf pas_conf =
    "<"
    ^ Int.IntLang.string_of_interactive_env pas_conf.ienv
    ^ " | "
    ^ Int.IntLang.Memory.string_of_memory pas_conf.memory
    ^ " > | "
    ^ Int.string_of_interactive_ctx pas_conf.ictx

  let p_trans act_conf =
    let nf_option =
      Int.IntLang.compute_nf (act_conf.computation, act_conf.memory) in
    match nf_option with
    | None -> (Int.Actions.diverging_action, None)
    | Some nf when Int.IntLang.is_error nf -> (Int.Actions.error_action, None)
    | Some nf ->
        let memory = Int.IntLang.get_memory nf in
                (* All the memory is supposed to be disclosed*)
        let memoryctx = Int.IntLang.Memory.infer_type_memory memory in
        let ictx = Int.replace_storectx act_conf.ictx memoryctx in
        let (move, ienv, lnamectx,ictx) = Int.generate_output_move ictx nf in
        (* We reset the P-name context of ictx using lnamectx*)
        let ictx = Int.replace_namectxP ictx lnamectx in
        (Int.Actions.inject_move move, Some { ienv; memory; ictx })

  let o_trans pas_conf input_move =
    match Int.check_input_move pas_conf.ictx input_move with
    | None -> None
    | Some ictx ->
        let (computation, new_memory, _) =
          Int.trigger_computation pas_conf.ienv input_move in
        let memory = Int.IntLang.Memory.update_memory pas_conf.memory new_memory in
        Some { computation; memory; ictx }

  let o_trans_gen pas_conf =
    let* (input_move, ictx) = Int.generate_input_moves pas_conf.ictx in
    let (computation, memory, _) =
      Int.trigger_computation pas_conf.ienv input_move in
    (*we throw away the interactive environment γ from trigger_computation, since we
      do not have interactive environment in active configurations of POGS. *)
    return (input_move, { computation; memory; ictx })

  let init_aconf computation namectxO =
    let ictx =
      Int.init_interactive_ctx Int.IntLang.Memory.empty_memory_type_ctx
        Int.IntLang.empty_name_ctx namectxO in
    { computation; memory= Int.IntLang.Memory.empty_memory; ictx }

  let init_pconf memory ienv namectxP namectxO =
    let memory_ctx = Int.IntLang.Memory.infer_type_memory memory in
    let ictx = Int.init_interactive_ctx memory_ctx namectxP namectxO in
    { memory ;ienv; ictx }

  let equiv_aconf aconf aconfb =
    aconf.computation = aconfb.computation && aconf.memory = aconfb.memory
end
