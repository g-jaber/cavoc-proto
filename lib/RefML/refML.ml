module Names : Lang.Names.NAMES with type name = Names.name = struct
  include Names
end

module Typed :
  Lang.Language.TYPED
    with type typ = Types.typ
     and type negative_type = Types.negative_type
     and type Namectx.t = Namectx.Namectx.t
     and module Names = Names = struct
  module Names = Names

  type typ = Types.typ
  let typ_to_yojson = Types.typ_to_yojson

  let string_of_type = Types.string_of_typ
  let pp_type = Types.pp_typ

  type negative_type = Types.negative_type
  let negative_type_to_yojson = Types.negative_type_to_yojson

  let string_of_negative_type = Types.string_of_negative_type
  let pp_negative_type = Types.pp_negative_type
  let get_negative_type = Types.get_negative_type

  module Namectx = Namectx.Namectx
end

module MakeStore (BranchMonad : Util.Monad.BRANCH) :
  Lang.Language.STORE
    with type store = Store.store
     and type label = Syntax.label
     and type Storectx.t = Store.Storectx.t
     and type location = Store.location
     and module BranchMonad = BranchMonad = struct
  include Store_gen.Make (BranchMonad)
end

module MakeComp (BranchMonad : Util.Monad.BRANCH) :
  Lang.Language.COMP
    with type term = Syntax.term
     and type value = Syntax.value
     and type negative_val = Syntax.negative_val
     and type typ = Types.typ
     and type negative_type = Types.negative_type
     and type Store.label = Syntax.label
     and type Store.Storectx.t = Store.Storectx.t
     and type Namectx.t = Namectx.Namectx.t
     and type IEnv.t = Ienv.IEnv.t
     and module Names = Names
     and module Store.BranchMonad = BranchMonad = struct
  include Syntax
  include Typed
  module Store = MakeStore (BranchMonad)

  module EvalMonad = Util.Monad.Option

  module IEnv = Ienv.IEnv


  type opconf = Interpreter.opconf

  let pp_opconf fmt (term, store) =
    Format.fprintf fmt "@[(@[Computation: %a@] @| @[Store: %a@])@]" pp_term term
      Store.pp_store store

  let normalize_opconf = Interpreter.normalize_opconf

  let get_typed_opconf nbprog lexBuffer =
    try
      let expr = Parser.fullexpr Lexer.token lexBuffer in
      let type_ctx = Type_ctx.build_type_ctx expr in
      let (type_ctx, ty) = Type_checker.typing_expr type_ctx expr in
      Util.Debug.print_debug
        ("Type checking of " ^ Syntax.string_of_term expr ^ " provides "
       ^ Types.string_of_typ ty);
      ((expr, Store.empty_store), ty, Type_ctx.get_name_ctx type_ctx)
    with
    | Lexer.SyntaxError msg ->
        failwith ("Lexing Error in the " ^ nbprog ^ " program:" ^ msg)
    | Parser.Error ->
        failwith
          ("Parsing Error in the " ^ nbprog ^ " program:" ^ " at position "
          ^ string_of_int (Lexing.lexeme_start lexBuffer))
    | Type_checker.TypingError msg ->
        failwith ("Typing Error in the " ^ nbprog ^ " program:" ^ msg)

  let get_typed_ienv lexBuffer_implem lexBuffer_signature =
    try
      let implem_decl_l = Parser.prog Lexer.token lexBuffer_implem in
      let signature_decl_l = Parser.signature Lexer.token lexBuffer_signature in
      let (comp_env, namectxO, cons_ctx) =
        Declaration.get_typed_comp_env implem_decl_l signature_decl_l in
      let (val_assign, heap, cons_ctx') =
        Interpreter.normalize_term_env cons_ctx comp_env in
      let (ienv, namectxP) =
        Declaration.get_typed_val_env val_assign signature_decl_l in
      (ienv, (val_assign, heap, cons_ctx'), namectxP, namectxO)
    with
    | Lexer.SyntaxError msg -> failwith ("Lexing Error: " ^ msg)
    | Parser.Error ->
        let pos = Lexing.lexeme_start_p lexBuffer_implem in
        let pos_str =
          Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
            (pos.pos_cnum - pos.pos_bol + 1) in
        failwith ("Parsing Errorr at " ^ pos_str)
        (* Need to get in which file the Parser.Error is *)
    | Type_checker.TypingError msg -> failwith ("Typing Error: " ^ msg)
end

module WithAVal (BranchMonad : Util.Monad.BRANCH) :
  Lang.Language.WITHAVAL_INOUT = struct
  include MakeComp (BranchMonad)

  type eval_context = Syntax.eval_context  [@@deriving to_yojson]

  let pp_eval_context = Syntax.pp_eval_context
  let string_of_eval_context = Syntax.string_of_eval_context

  type typevar = Types.typevar [@@deriving to_yojson]
  type typename = Types.id

  let string_of_typename id = id
  let pp_tvar_l = Types.pp_tvar_l

  let generate_typename_subst tvar_l =
    let aux tvar =
      let tname = Types.fresh_typename () in
      (tname, (tvar, Types.TName tname)) in
    let (tname_l, type_subst_l) = List.split @@ List.map aux tvar_l in
    (tname_l, Util.Pmap.list_to_pmap type_subst_l)

  let apply_type_subst = Types.apply_type_subst
  let get_input_type = Types.get_input_type
  let get_output_type = Types.get_output_type

  module Nf_gen = Nf.Make (BranchMonad)

  module Nf = struct
    include Nf
    module BranchMonad = BranchMonad

    let abstract_nf_term_m = Nf_gen.abstract_nf_term_m
  end

  let type_annotating_val = Nf.type_annotating_val
  let type_annotating_ectx = Nf.type_annotating_ectx
  let type_check_nf_term = Nf.type_check_nf_term
  let generate_nf_term_call = Nf_gen.generate_nf_term_call
  let generate_nf_term_ret = Nf_gen.generate_nf_term_ret

  type normal_form_term = (value, eval_context, Names.name, unit) Nf.nf_term

  let refold_nf_term = Syntax.refold_nf_term
  let get_nf_term = Syntax.get_nf_term

  (*
  let generate_nf_skeleton = Nf_gen.generate_nf_skeleton
  let fill_nf_skeleton = Nf_gen.fill_nf_skeleton*)

  module AVal :
    Lang.Abstract_val.AVAL
      with type name = Names.name
       and type value = Syntax.value
       and type negative_val = Syntax.negative_val
       and type typ = Types.typ
       and type negative_type = Types.negative_type
       and type label = Syntax.label
       and type store_ctx = Store.Storectx.t
       and type name_ctx = Namectx.t
       and type interactive_env = Ienv.IEnv.t
       and module BranchMonad = BranchMonad =
    Nup.Make (BranchMonad)
end
