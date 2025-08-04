module Make (Int : Lts.Interactive.INT) = struct
  module M = Int.IntLang.M
  module EvalMonad = Int.IntLang.EvalMonad
  module Int = Int
  module Moves = Int.Moves

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

  let passive_conf_to_yojson passive_conf =
    `Assoc
      [
        ("store", `String (Int.IntLang.Store.string_of_store passive_conf.store));
        ("ienv", Int.IntLang.interactive_env_to_yojson passive_conf.ienv);
        ("ictx", Int.interactive_ctx_to_yojson passive_conf.ictx);
      ]

  type conf = Active of active_conf | Passive of passive_conf

  let pp_active_conf fmt act_conf =
    Format.fprintf fmt
      "@[⟨@[Computation: %a@] @,\
      \ @[Store: %a@] @,\
      \ @[IEnv:  %a@] @,\
      \ @[ICtx: %a@]⟩@]"
      Int.IntLang.pp_computation act_conf.computation Int.IntLang.Store.pp_store
      act_conf.store Int.IntLang.pp_ienv act_conf.ienv Int.pp_interactive_ctx
      act_conf.ictx

  let pp_passive_conf fmt pas_conf =
    Format.fprintf fmt "@[⟨@[Store: %a@] @, @[IEnv:  %a@] @, @[ICtx: %a@]⟩@]"
      Int.IntLang.Store.pp_store pas_conf.store Int.IntLang.pp_ienv
      pas_conf.ienv Int.pp_interactive_ctx pas_conf.ictx

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let extract_interactive_ctx = function
    | Active a_iconf -> a_iconf.ictx
    | Passive p_iconf -> p_iconf.ictx

  let p_trans act_conf =
    let open EvalMonad in
    let* nf =
      Int.IntLang.compute_nf (act_conf.computation, act_conf.store) in
      if Int.IntLang.is_error nf then fail () (* to be improved *)
      else 
        let* (move, ienv', _, ictx) = Int.generate_output_move act_conf.ictx nf in
        let ienv = Int.IntLang.concat_ienv ienv' act_conf.ienv in
        let store = Int.IntLang.get_store nf in
        return (move, { store; ienv; ictx })

  let o_trans pas_conf input_move =
    match Int.check_input_move pas_conf.ictx input_move with
    | None -> None
    | Some ictx ->
        let (computation, store', ienv) =
          Int.trigger_computation pas_conf.ienv input_move in
        let store = Int.IntLang.Store.update_store pas_conf.store store' in
        Some { computation; store; ienv; ictx }

  let o_trans_gen pas_conf =
    let open M in
    let* (input_move, ictx) = Int.generate_input_moves pas_conf.ictx in
    let (computation, store', ienv) =
      Int.trigger_computation pas_conf.ienv input_move in
    Util.Debug.print_debug "Updating the store";
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
    let ictx = Int.init_interactive_ctx store_ctx namectxP namectxO in
    { store; ienv; ictx }

  let equiv_act_conf act_conf act_confb =
    act_conf.computation = act_confb.computation
    && act_conf.store = act_confb.store
end
