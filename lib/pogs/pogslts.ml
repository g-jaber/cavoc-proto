module Make (Int : Lts.Interactive.INT) :
  Lts.Bipartite.INT_LTS
    with module Int = Int
     and module OBranchingMonad = Int.GameLTS.BranchMonad = struct
  module OBranchingMonad = Int.GameLTS.BranchMonad
  module EvalMonad = Int.IntLang.EvalMonad
  module Int = Int
  module Moves = Int.GameLTS.Moves

  type active_conf = {
    opconf: Int.IntLang.opconf;
    ictx: Int.GameLTS.position;
  }

  type passive_conf = {
    ienv: Int.IntLang.interactive_env;
    store: Int.IntLang.store;
    ictx: Int.GameLTS.position;
  }

  type conf = Active of active_conf | Passive of passive_conf

  let passive_conf_to_yojson passive_conf =
    `Assoc
      [
        ("store", `String (Int.IntLang.string_of_store passive_conf.store));
        ("ienv", Int.IntLang.interactive_env_to_yojson passive_conf.ienv);
        ("ictx", Int.GameLTS.position_to_yojson passive_conf.ictx);
      ]

  let pp_active_conf fmt act_conf =
    Format.fprintf fmt "@[⟨%a |@, %a ⟩@]" Int.IntLang.pp_opconf act_conf.opconf
      Int.GameLTS.pp_position act_conf.ictx

  let pp_passive_conf fmt pas_conf =
    Format.fprintf fmt "@[⟨%a |@, %a |@, %a⟩]" Int.IntLang.pp_store
      pas_conf.store Int.IntLang.pp_ienv pas_conf.ienv
      Int.GameLTS.pp_position pas_conf.ictx

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let p_trans act_conf =
    let open EvalMonad in
    let* nf = Int.IntLang.compute_nf act_conf.opconf in
    if Int.IntLang.is_error nf then fail () (* to be improved *)
    else
      let store = Int.IntLang.get_store nf in
      (* All the store is supposed to be disclosed*)
      let storectx = Int.IntLang.infer_type_store store in
      let ictx = Int.GameLTS.replace_storectx act_conf.ictx storectx in
      let* (move, ienv, lnamectx, ictx) = Int.generate_output_move ictx nf in
      (* We reset the P-name context of ictx using lnamectx*)
      let ictx = Int.GameLTS.replace_namectxP ictx lnamectx in
      return (move, { ienv; store; ictx })

  let o_trans pas_conf input_move =
    match Int.GameLTS.check_move pas_conf.ictx input_move with
    | None -> None
    | Some ictx ->
        let (opconf, _) =
          Int.trigger_computation pas_conf.store pas_conf.ienv input_move in
        Some { opconf; ictx }

  let o_trans_gen pas_conf =
    let open OBranchingMonad in
    let* (input_move, ictx) = Int.GameLTS.generate_moves pas_conf.ictx in
    let (opconf, _) =
      Int.trigger_computation pas_conf.store pas_conf.ienv input_move in
    (*we throw away the interactive environment γ from trigger_computation, since we
      do not have interactive environment in active configurations of POGS. *)
    return (input_move, { opconf; ictx })

  let init_aconf opconf namectxO =
    let ictx =
      Int.GameLTS.init_position Int.IntLang.empty_store_ctx
        Int.IntLang.empty_name_ctx namectxO in
    { opconf; ictx }

  let init_pconf store ienv namectxP namectxO =
    let store_ctx = Int.IntLang.infer_type_store store in
    let ictx = Int.GameLTS.init_position store_ctx namectxP namectxO in
    { store; ienv; ictx }

  let equiv_act_conf act_conf act_confb =
    act_conf.opconf = act_confb.opconf (* Fishy *)
end
