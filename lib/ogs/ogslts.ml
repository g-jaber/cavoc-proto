(* The OGS Strategy over an interactive language. 
  MakeWithInit adds the lexbuf-based
  initialization for languages that parse their initial configurations. *)
module Make
    (Lang : Lang.Interactive.LANG)
    (TypingLTS :
      Lts.Typing.LTS
        with module Moves.Renaming = Lang.IEnv.Renaming
         and type Moves.copattern =
          Lang.abstract_normal_form * Lang.IEnv.Renaming.t
         and type store_ctx = Lang.Storectx.t) : sig
  include
    Lts.Strategy.LTS
      with module TypingLTS = TypingLTS
       and module EvalMonad = Lang.EvalMonad

  val init_aconf : Lang.opconf -> Lang.IEnv.Renaming.Namectx.t -> active_conf

  val init_pconf :
    Lang.store ->
    Lang.IEnv.t ->
    Lang.IEnv.Renaming.Namectx.t ->
    Lang.IEnv.Renaming.Namectx.t ->
    passive_conf
end = struct
  module TypingLTS = TypingLTS
  module EvalMonad = Lang.EvalMonad
  module Moves = TypingLTS.Moves

  type active_conf = {
    opconf: Lang.opconf;
    ienv: Lang.IEnv.t;
    pos: TypingLTS.position;
  }

  type passive_conf = {
    store: Lang.store;
    ienv: Lang.IEnv.t;
    pos: TypingLTS.position;
  }

  let passive_conf_to_yojson passive_conf =
    `Assoc
      [
        ("store", Lang.store_to_yojson passive_conf.store);
        ("ienv", Lang.IEnv.to_yojson passive_conf.ienv);
        ("pos", TypingLTS.position_to_yojson passive_conf.pos);
      ]

  type conf = Active of active_conf | Passive of passive_conf

  let pp_active_conf fmt act_conf =
    Format.fprintf fmt "@[⟨@[OpConf: %a@] @, @[IEnv:  %a@] @, @[ICtx: %a@]⟩@]"
      Lang.pp_opconf act_conf.opconf Lang.IEnv.pp act_conf.ienv
      TypingLTS.pp_position act_conf.pos

  let pp_passive_conf fmt pas_conf =
    Format.fprintf fmt "@[⟨@[Store: %a@] @, @[IEnv:  %a@] @, @[ICtx: %a@]⟩@]"
      Lang.pp_store pas_conf.store Lang.IEnv.pp pas_conf.ienv
      TypingLTS.pp_position pas_conf.pos

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf
  let get_active_pos (act_conf : active_conf) = act_conf.pos
  let get_passive_pos (pas_conf : passive_conf) = pas_conf.pos

  let p_trans (act_conf : active_conf) =
    let open EvalMonad in
    (* γ : Γₚ → Γₒ, substitution convention: dom = the P-names γ defines,
       im = the O-names its values may mention. *)
    let namectxO = Lang.IEnv.im act_conf.ienv in
    let* ((a_nf, lnamectx, _storectx_discl), ienv, store) =
      Lang.eval (act_conf.opconf, namectxO, TypingLTS.get_storectx act_conf.pos)
    in
    let move =
      ( TypingLTS.Moves.Output,
        TypingLTS.weaken_move act_conf.pos TypingLTS.Moves.Output
          (a_nf, TypingLTS.Moves.Renaming.id lnamectx) ) in
    let pos = TypingLTS.trigger_move act_conf.pos move in
    let ienv = Lang.IEnv.copairing act_conf.ienv ienv in
    (* The weakening of the move and the ienv-domain extension performed by
       copairing must agree, up to display hints. *)
    assert (
      TypingLTS.Moves.Renaming.Namectx.to_pmap (TypingLTS.get_namectxP pos)
      = TypingLTS.Moves.Renaming.Namectx.to_pmap (Lang.IEnv.dom ienv));
    return (move, { store; ienv; pos })

  let o_trans pas_conf ((_, a_nf) as input_move) =
    match TypingLTS.check_move pas_conf.pos input_move with
    | None -> None
    | Some pos ->
        let (opconf, ienv) =
          Lang.concretize_a_nf pas_conf.store pas_conf.ienv a_nf in
        Some { opconf; ienv; pos }

  let o_trans_gen pas_conf =
    let open TypingLTS.BranchMonad in
    let* (((_, a_nf) as input_move), pos) =
      TypingLTS.generate_moves pas_conf.pos in
    let (opconf, ienv) =
      Lang.concretize_a_nf pas_conf.store pas_conf.ienv a_nf in
    return (input_move, { opconf; ienv; pos })

  let init_aconf opconf namectxO =
    let pos =
      TypingLTS.init_act_pos Lang.Storectx.empty
        Lang.IEnv.Renaming.Namectx.empty namectxO in
    { opconf; ienv= Lang.IEnv.empty namectxO; pos }

  let init_pconf store ienv namectxP namectxO =
    let store_ctx = Lang.Storectx.empty in
    (* we suppose that the initial store is not shared *)
    (* TODO: Why? *)
    let pos = TypingLTS.init_pas_pos store_ctx namectxP namectxO in
    { store; ienv; pos }

  let equiv_act_conf act_conf act_confb =
    act_conf.opconf = act_confb.opconf (* That's fishy *)
end

module MakeWithInit
    (Lang : Lang.Interactive.LANG_WITH_INIT)
    (TypingLTS :
      Lts.Typing.LTS
        with module Moves.Renaming = Lang.IEnv.Renaming
         and type Moves.copattern =
          Lang.abstract_normal_form * Lang.IEnv.Renaming.t
         and type store_ctx = Lang.Storectx.t) :
  Lts.Strategy.LTS_WITH_INIT
    with module TypingLTS = TypingLTS
     and module TypingLTS.Moves.Renaming = Lang.IEnv.Renaming
     and module EvalMonad = Lang.EvalMonad = struct
  include Make (Lang) (TypingLTS)

  let lexing_init_aconf expr_lexbuffer =
    let (opconf, namectxO) = Lang.get_typed_opconf "first" expr_lexbuffer in
    init_aconf opconf namectxO

  let lexing_init_pconf ?opponent_signature decl_lexbuffer signature_lexbuffer =
    let (interactive_env, store, name_ctxP, name_ctxO) =
      Lang.get_typed_ienv ?opponent_signature decl_lexbuffer signature_lexbuffer
    in
    init_pconf store interactive_env name_ctxP name_ctxO
end
