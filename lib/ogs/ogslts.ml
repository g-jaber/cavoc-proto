module Make (Int : Lts.Interactive.INT) = struct
  module M = Int.IntLang.M
  include M
  module Int = Int
  module Actions = Int.Actions

  type active_conf = {
    computation: Int.IntLang.computation;
    store: Int.IntLang.Store.store;
    ienv: Int.IntLang.interactive_env;
    ictx: Int.interactive_ctx;
  }

  type passive_conf = {
    store: Int.IntLang.Store.store;
    ienv: Int.IntLang.interactive_env;
    ictx: Int.interactive_ctx;
  }

  type conf = Active of active_conf | Passive of passive_conf

  let string_of_active_conf act_conf =
    "<"
    ^ Int.IntLang.string_of_computation act_conf.computation
    ^ " | "
    ^ Int.IntLang.Store.string_of_store act_conf.store
    ^ " | "
    ^ Int.IntLang.string_of_interactive_env act_conf.ienv
    ^ " > | "
    ^ Int.string_of_interactive_ctx act_conf.ictx

  let string_of_passive_conf pas_conf =
    "<"
    ^ Int.IntLang.Store.string_of_store pas_conf.store
    ^ " | "
    ^ Int.IntLang.string_of_interactive_env pas_conf.ienv
    ^ " > | "
    ^ Int.string_of_interactive_ctx pas_conf.ictx

  let p_trans act_conf =
    let nf_option =
      Int.IntLang.compute_nf (act_conf.computation, act_conf.store) in
    match nf_option with
    | None -> (Int.Actions.diverging_action, None)
    | Some nf when Int.IntLang.is_error nf -> (Int.Actions.error_action, None)
    | Some nf ->
        let (move, ienv', _,ictx) =
          Int.generate_output_move act_conf.ictx nf in
        let ienv = Int.IntLang.concat_ienv ienv' act_conf.ienv in
        let store = Int.IntLang.get_store nf in
        (Int.Actions.inject_move move, Some { store; ienv; ictx })

  let o_trans pas_conf input_move =
    match Int.check_input_move pas_conf.ictx input_move with
    | None -> None
    | Some ictx ->
        let (computation, store', ienv) =
          Int.trigger_computation pas_conf.ienv input_move in
        let store = Int.IntLang.Store.update_store pas_conf.store store' in
        Some { computation; store; ienv; ictx }

  let o_trans_gen pas_conf =
    let* (input_move, ictx) = Int.generate_input_moves pas_conf.ictx in
    let (computation, store', ienv) =
      Int.trigger_computation pas_conf.ienv input_move in
    let store = Int.IntLang.Store.update_store pas_conf.store store' in
    return (input_move, { computation; store; ienv; ictx })

  let init_aconf computation namectxO =
    let ictx =
      Int.init_interactive_ctx Int.IntLang.Store.empty_store_ctx
        Int.IntLang.empty_name_ctx namectxO in
    {
      computation;
      store= Int.IntLang.Store.empty_store;
      ienv= Int.IntLang.empty_ienv;
      ictx;
    }

  let init_pconf store ienv namectxP namectxO =
    let store_ctx = Int.IntLang.Store.empty_store_ctx in
    (* we suppose that the initial store is not shared *)
    let ictx =
      Int.init_interactive_ctx store_ctx namectxP
        namectxO in
    { store= store; ienv; ictx }

    let equiv_act_conf act_conf act_confb =
      act_conf.computation = act_confb.computation && act_conf.store = act_confb.store
end
