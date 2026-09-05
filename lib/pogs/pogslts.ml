(* Opponent chooses the whole heap over the cells of ground type when it
   plays, andPlayer answers with its whole heap. *)
module Make (Lang : Lang.Interactive.LANG_WITH_INIT) : sig
  include
    Lts.Strategy.LTS_WITH_INIT
      with module EvalMonad = Lang.EvalMonad
       and type TypingLTS.store_ctx = Lang.Storectx.t

  val get_passive_storectx_discl : passive_conf -> Lang.Storectx.t
end = struct
  module TypingLTS = Typing.Make (Lang)
  module EvalMonad = Lang.EvalMonad
  module Moves = TypingLTS.Moves

  type active_conf = {
    opconf: Lang.opconf;
    ienv: Lang.IEnv.t;
    pos: TypingLTS.position;
    storectx_discl: Lang.Storectx.t;
  }

  type passive_conf = {
    ienv: Lang.IEnv.t;
    store: Lang.store;
    pos: TypingLTS.position;
    storectx_discl: Lang.Storectx.t;
  }

  type conf = Active of active_conf | Passive of passive_conf

  let passive_conf_to_yojson (passive_conf : passive_conf) =
    `Assoc
      [
        ("store", Lang.store_to_yojson passive_conf.store);
        ("ienv", Lang.IEnv.to_yojson passive_conf.ienv);
        ("pos", TypingLTS.position_to_yojson passive_conf.pos);
        ("storectx_discl", Lang.Storectx.to_yojson passive_conf.storectx_discl);
      ]

  let pp_active_conf fmt (act_conf : active_conf) =
    Format.fprintf fmt "@[⟨%a |@, %a |@, %a⟩@]" Lang.pp_opconf act_conf.opconf
      Lang.IEnv.pp act_conf.ienv TypingLTS.pp_position act_conf.pos

  let pp_passive_conf fmt (pas_conf : passive_conf) =
    Format.fprintf fmt "@[⟨%a |@, %a |@, %a⟩]" Lang.pp_store pas_conf.store
      Lang.IEnv.pp pas_conf.ienv TypingLTS.pp_position pas_conf.pos

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf
  let get_active_pos (act_conf : active_conf) = act_conf.pos
  let get_passive_pos (pas_conf : passive_conf) = pas_conf.pos

  let get_passive_storectx_discl (pas_conf : passive_conf) =
    pas_conf.storectx_discl

  let p_trans (act_conf : active_conf) =
    let open EvalMonad in
    let namectxO = Lang.IEnv.im act_conf.ienv in
    let* ((a_nf, lnamectx, storectx_discl), ienv, store) =
      Lang.eval (act_conf.opconf, namectxO, act_conf.storectx_discl) in
    let a_nf = Lang.replace_store_of_a_nf a_nf store in
    let move = (TypingLTS.Moves.Output, (a_nf, lnamectx)) in
    let (_, pos) = TypingLTS.trigger_move act_conf.pos move in
    let pos = TypingLTS.replace_storectx (Lang.infer_type_store store) pos in
    let ienv = Lang.IEnv.copairing act_conf.ienv ienv in
    return (move, ({ store; ienv; pos; storectx_discl } : passive_conf))

  (* An Input move keeps the environment of the type and polymorphic names
     only, in step with the Player context of the position it reaches. *)
  let concretize (pas_conf : passive_conf) a_nf weakening =
    let (opconf, ienv) =
      Lang.concretize_a_nf pas_conf.store pas_conf.ienv (a_nf, weakening) in
    let reset_thinning =
      TypingLTS.reset_thinning (TypingLTS.get_namectxP pas_conf.pos) in
    (opconf, Lang.IEnv.restrict reset_thinning ienv)

  let o_trans (pas_conf : passive_conf) ((_, (a_nf, _)) as input_move) =
    match TypingLTS.check_move pas_conf.pos input_move with
    | None -> None
    | Some (weakening, pos) ->
        let (opconf, ienv) = concretize pas_conf a_nf weakening in
        Some
          ({ opconf; ienv; pos; storectx_discl= pas_conf.storectx_discl }
            : active_conf)

  let o_trans_gen (pas_conf : passive_conf) =
    let open TypingLTS.BranchMonad in
    let* (((_, (a_nf, _)) as input_move), weakening, pos) =
      TypingLTS.generate_moves pas_conf.pos TypingLTS.Moves.Input in
    let (opconf, ienv) = concretize pas_conf a_nf weakening in
    return
      ( input_move,
        ({ opconf; ienv; pos; storectx_discl= pas_conf.storectx_discl }
          : active_conf) )

  let init_aconf opconf namectxO =
    let pos =
      TypingLTS.init_act_pos Lang.Storectx.empty
        Lang.IEnv.Renaming.Namectx.empty namectxO in
    {
      opconf;
      ienv= Lang.IEnv.empty namectxO;
      pos;
      storectx_discl= Lang.Storectx.empty;
    }

  let init_pconf store ienv namectxP namectxO =
    let store_ctx = Lang.infer_type_store store in
    let pos = TypingLTS.init_pas_pos store_ctx namectxP namectxO in
    { store; ienv; pos; storectx_discl= Lang.Storectx.empty }

  let equiv_act_conf (act_conf : active_conf) (act_confb : active_conf) =
    act_conf.opconf = act_confb.opconf (* Fishy *)

  let lexing_init_aconf expr_lexbuffer =
    let (opconf, namectxO) = Lang.get_typed_opconf "first" expr_lexbuffer in
    init_aconf opconf namectxO

  let lexing_init_pconf ?opponent_signature decl_lexbuffer signature_lexbuffer =
    let (interactive_env, store, name_ctxP, name_ctxO) =
      Lang.get_typed_ienv ?opponent_signature decl_lexbuffer signature_lexbuffer
    in
    init_pconf store interactive_env name_ctxP name_ctxO
end
