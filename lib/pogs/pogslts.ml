module Make (Lang : Lang.Interactive.LANG) :
  Lts.Bipartite.INT_LTS
    with type name_ctx = Lang.Namectx.t
     and type opconf = Lang.opconf
     and type store = Lang.store
     and type interactive_env = Lang.interactive_env = struct
  module TypingLTS = Typing.Make (Lang)
  module OBranchingMonad = TypingLTS.BranchMonad
  module EvalMonad = Lang.EvalMonad
  module Moves = TypingLTS.Moves

  type name_ctx = TypingLTS.name_ctx
  type opconf = Lang.opconf
  type store = Lang.store
  type interactive_env = Lang.interactive_env

  let get_names = Lang.Namectx.get_names

  type active_conf = { opconf: Lang.opconf; pos: TypingLTS.position }

  type passive_conf = {
    ienv: Lang.interactive_env;
    store: Lang.store;
    pos: TypingLTS.position;
  }

  type conf = Active of active_conf | Passive of passive_conf

  let passive_conf_to_yojson passive_conf =
    `Assoc
      [
        ("store", `String (Lang.string_of_store passive_conf.store));
        ("ienv", Lang.interactive_env_to_yojson passive_conf.ienv);
        ("pos", TypingLTS.position_to_yojson passive_conf.pos);
      ]

  let pp_active_conf fmt act_conf =
    Format.fprintf fmt "@[⟨%a |@, %a ⟩@]" Lang.pp_opconf act_conf.opconf
      TypingLTS.pp_position act_conf.pos

  let pp_passive_conf fmt pas_conf =
    Format.fprintf fmt "@[⟨%a |@, %a |@, %a⟩]" Lang.pp_store pas_conf.store
      Lang.pp_ienv pas_conf.ienv TypingLTS.pp_position pas_conf.pos

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let p_trans act_conf =
    let open EvalMonad in
    let* ((a_nf, lnamectx, _storectx_discl), ienv, store) =
      Lang.eval
        ( act_conf.opconf,
          TypingLTS.get_namectxO act_conf.pos,
          TypingLTS.get_storectx act_conf.pos ) in
    let move = (TypingLTS.Moves.Output, a_nf) in
    let pos = TypingLTS.trigger_move act_conf.pos (move, lnamectx) in
    return ((move, lnamectx), { store; ienv; pos })

  let o_trans pas_conf (((_, move) as input_move), namectx) =
    match TypingLTS.check_move pas_conf.pos (input_move, namectx) with
    | None -> None
    | Some pos ->
        let (opconf, _) =
          Lang.concretize_a_nf pas_conf.store pas_conf.ienv move in
        Some { opconf; pos }

  let o_trans_gen pas_conf =
    let open OBranchingMonad in
    let* ((((_, move) as input_move), namectx), pos) =
      TypingLTS.generate_moves pas_conf.pos in
    let (opconf, _) = Lang.concretize_a_nf pas_conf.store pas_conf.ienv move in
    (*we throw away the interactive environment γ from trigger_computation, since we
      do not have interactive environment in active configurations of POGS. *)
    return ((input_move, namectx), { opconf; pos })

  let init_aconf opconf namectxO =
    let pos =
      TypingLTS.init_act_pos Lang.Storectx.empty Lang.Namectx.empty namectxO
    in
    { opconf; pos }

  let init_pconf store ienv namectxP namectxO =
    let store_ctx = Lang.infer_type_store store in
    let pos = TypingLTS.init_pas_pos store_ctx namectxP namectxO in
    { store; ienv; pos }

  let equiv_act_conf act_conf act_confb =
    act_conf.opconf = act_confb.opconf (* Fishy *)
end
