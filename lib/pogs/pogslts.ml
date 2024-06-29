module Make (Int : Lts.Interactive.INT) = struct
  module M = Int.IntLang.M
  include M
  module Int = Int
  module Actions = Int.Actions

  type active_conf = {
    computation: Int.IntLang.computation;
    store: Int.IntLang.Store.store;
    ictx: Int.interactive_ctx;
  }

  type passive_conf = {
    ienv: Int.IntLang.interactive_env;
    store: Int.IntLang.Store.store;
    ictx: Int.interactive_ctx;
  }

  type conf = Active of active_conf | Passive of passive_conf

  let string_of_active_conf act_conf =
    "<"
    ^ Int.IntLang.string_of_computation act_conf.computation
    ^ " | "
    ^ Int.IntLang.Store.string_of_store act_conf.store
    ^ " > | "
    ^ Int.string_of_interactive_ctx act_conf.ictx

  let string_of_passive_conf pas_conf =
    "<"
    ^ Int.IntLang.string_of_ienv pas_conf.ienv
    ^ " | "
    ^ Int.IntLang.Store.string_of_store pas_conf.store
    ^ " > | "
    ^ Int.string_of_interactive_ctx pas_conf.ictx

  let extract_interactive_ctx = function
    | Active a_iconf -> a_iconf.ictx
    | Passive p_iconf -> p_iconf.ictx

  let p_trans act_conf =
    let nf_option =
      Int.IntLang.compute_nf (act_conf.computation, act_conf.store) in
    match nf_option with
    | None -> (Int.Actions.diverging_action, None)
    | Some nf when Int.IntLang.is_error nf -> (Int.Actions.error_action, None)
    | Some nf ->
        let store = Int.IntLang.get_store nf in
        (* All the store is supposed to be disclosed*)
        let storectx = Int.IntLang.Store.infer_type_store store in
        let ictx = Int.replace_storectx act_conf.ictx storectx in
        let (move, ienv, lnamectx, ictx) = Int.generate_output_move ictx nf in
        (* We reset the P-name context of ictx using lnamectx*)
        let ictx = Int.replace_namectxP ictx lnamectx in
        (Int.Actions.inject_move move, Some { ienv; store; ictx })

  let o_trans pas_conf input_move =
    match Int.check_input_move pas_conf.ictx input_move with
    | None -> None
    | Some ictx ->
        let (computation, new_store, _) =
          Int.trigger_computation pas_conf.ienv input_move in
        let store = Int.IntLang.Store.update_store pas_conf.store new_store in
        Some { computation; store; ictx }

  let o_trans_gen pas_conf =
    let* (input_move, ictx) = Int.generate_input_moves pas_conf.ictx in
    let (computation, store, _) =
      Int.trigger_computation pas_conf.ienv input_move in
    (*we throw away the interactive environment γ from trigger_computation, since we
      do not have interactive environment in active configurations of POGS. *)
    return (input_move, { computation; store; ictx })

  let init_aconf computation namectxO =
    let ictx =
      Int.init_interactive_ctx Int.IntLang.Store.empty_store_ctx
        Int.IntLang.empty_name_ctx namectxO in
    { computation; store= Int.IntLang.Store.empty_store; ictx }

  let init_pconf store ienv namectxP namectxO =
    let store_ctx = Int.IntLang.Store.infer_type_store store in
    let ictx = Int.init_interactive_ctx store_ctx namectxP namectxO in
    { store; ienv; ictx }

  let equiv_act_conf act_conf act_confb =
    act_conf.computation = act_confb.computation
    && act_conf.store = act_confb.store
end
