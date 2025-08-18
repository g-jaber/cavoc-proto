(*module Make (Lang : Lang.Interactive.LANG) (TypingLTS : Lts.Typing.LTS with type name_ctx = Lang.name_ctx and type store_ctx = Lang.store_ctx) :*)

module Make (Int : Lts.Interactive.INT) :
  Lts.Bipartite.INT_LTS
    with module OBranchingMonad = Int.GameLTS.BranchMonad
     and type name_ctx = Int.IntLang.name_ctx
     and type opconf = Int.IntLang.opconf
     and type store = Int.IntLang.store
     and type interactive_env = Int.IntLang.interactive_env = struct
  module OBranchingMonad = Int.GameLTS.BranchMonad
  module EvalMonad = Int.IntLang.EvalMonad
  module Moves = Int.GameLTS.Moves

  type name_ctx = Int.GameLTS.name_ctx
  type opconf = Int.IntLang.opconf
  type store = Int.IntLang.store
  type interactive_env = Int.IntLang.interactive_env

  let get_names_from_name_ctx = Int.IntLang.get_names_from_name_ctx

  type active_conf = {
    opconf: Int.IntLang.opconf;
    ienv: Int.IntLang.interactive_env;
    ictx: Int.GameLTS.position;
  }

  type passive_conf = {
    store: Int.IntLang.store;
    ienv: Int.IntLang.interactive_env;
    ictx: Int.GameLTS.position;
  }

  let passive_conf_to_yojson passive_conf =
    `Assoc
      [
        ("store", `String (Int.IntLang.string_of_store passive_conf.store));
        ("ienv", Int.IntLang.interactive_env_to_yojson passive_conf.ienv);
        ("ictx", Int.GameLTS.position_to_yojson passive_conf.ictx);
      ]

  type conf = Active of active_conf | Passive of passive_conf

  let pp_active_conf fmt act_conf =
    Format.fprintf fmt "@[⟨@[OpConf: %a@] @, @[IEnv:  %a@] @, @[ICtx: %a@]⟩@]"
      Int.IntLang.pp_opconf act_conf.opconf Int.IntLang.pp_ienv act_conf.ienv
      Int.GameLTS.pp_position act_conf.ictx

  let pp_passive_conf fmt pas_conf =
    Format.fprintf fmt "@[⟨@[Store: %a@] @, @[IEnv:  %a@] @, @[ICtx: %a@]⟩@]"
      Int.IntLang.pp_store pas_conf.store Int.IntLang.pp_ienv pas_conf.ienv
      Int.GameLTS.pp_position pas_conf.ictx

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let p_trans act_conf =
    let open EvalMonad in
    let* nf = Int.IntLang.compute_nf act_conf.opconf in
    if Int.IntLang.is_error nf then fail () (* to be improved *)
    else
      let* (move, ienv', ictx) = Int.generate_output_move act_conf.ictx nf in
      let ienv = Int.IntLang.concat_ienv ienv' act_conf.ienv in
      let store = Int.IntLang.get_store nf in
      return (move, { store; ienv; ictx })

  let o_trans pas_conf (input_move, namectx) =
    match Int.GameLTS.check_move pas_conf.ictx (input_move, namectx) with
    | None -> None
    | Some ictx ->
        let (opconf, ienv) =
          Int.trigger_computation pas_conf.store pas_conf.ienv input_move in
        Some { opconf; ienv; ictx }

  let o_trans_gen pas_conf =
    let open OBranchingMonad in
    let* ((input_move, namectx), ictx) =
      Int.GameLTS.generate_moves pas_conf.ictx in
    let (opconf, ienv) =
      Int.trigger_computation pas_conf.store pas_conf.ienv input_move in
    return ((input_move, namectx), { opconf; ienv; ictx })

  let init_aconf opconf namectxO =
    let ictx =
      Int.GameLTS.init_position Int.IntLang.empty_store_ctx
        Int.IntLang.empty_name_ctx namectxO in
    { opconf; ienv= Int.IntLang.empty_ienv; ictx }

  let init_pconf store ienv namectxP namectxO =
    let store_ctx = Int.IntLang.empty_store_ctx in
    (* we suppose that the initial store is not shared *)
    let ictx = Int.GameLTS.init_position store_ctx namectxP namectxO in
    { store; ienv; ictx }

  let equiv_act_conf act_conf act_confb =
    act_conf.opconf = act_confb.opconf (* That's fishy *)
end
