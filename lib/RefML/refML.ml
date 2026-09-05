(** This module contains the main implementations of the {e RefML}
    {{!page-index.machinelanguages}machine language}.

    Two implementations of {e RefML} are provided:
    - A {{!module: WithAValConcrete}concrete} {e RefML} implementation ;
    - A {{!module: WithAValSymbolic}symbolic} {e RefML} implementation based on
      symbolic execution.

    See {!module: Lang.Language} for more details about machine languages. *)

module Names : Lang.Names.NAMES with type name = Names.name = struct
  include Names
end

module Typed :
  Lang.Language.TYPED
    with type typ = Types.typ
     and type negative_type = Types.negative_type
     and type Namectx.t = Namectx.Namectx.t
     and type Renaming.t = Renaming.Renaming.t
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

  module Namectx = Namectx.Namectx
  module Renaming = Renaming.Renaming
end

module MakeStore (BranchMonad : Util.Monad.BRANCH) :
  Lang.Language.STORE
    with type store = Store.store
     and type label = Syntax.label
     and type Storectx.t = Store.Storectx.t
     and module BranchMonad = BranchMonad = struct
  include Store_gen.Make (BranchMonad)
end

let parse_and_handle_error parser_entry lexbuf =
  let format_msg msg =
    let pos = Lexing.lexeme_start_p lexbuf in
    Printf.sprintf "%s in %s at line %d, column %d" msg pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1) in
  try parser_entry Lexer.token lexbuf with
  | Lexer.SyntaxError msg -> failwith (format_msg ("Lexing Error: " ^ msg))
  | Parser.Error -> failwith (format_msg "Parsing Error")

(** [MakeCompBase] contains most of the code needed to implement a
    {{!page-index.machinelanguages}machine language} based on {e RefML}. Note
    that it doesn't directly implement any of the signatures from
    {!module: Lang.Language} as it lacks the machinery related to evaluation and
    generation of abstract values. *)
module MakeCompBase (BranchMonad : Util.Monad.BRANCH) = struct
  include Syntax
  include Typed
  module Store = MakeStore (BranchMonad)
  module IEnv = Ienv.IEnv

  type opconf = Interpreter.opconf

  let pp_opconf fmt (term, store) =
    Format.fprintf fmt "@[(@[Computation: %a@] @| @[Store: %a@])@]" pp_term term
      Store.pp_store store

  (* [opponent_signature] plays the same role as in [get_typed_ienv]: the declared
     names become Opponent names of the expression, and the identifiers
     resolve to them through the initial store's value environment. *)
  let get_typed_opconf ?opponent_signature nbprog lexBuffer =
    try
      let expr = parse_and_handle_error Parser.fullexpr lexBuffer in
      let import_decl_l =
        match opponent_signature with
        | None -> []
        | Some lexBuffer_imports ->
            parse_and_handle_error Parser.signature lexBuffer_imports in
      let (import_val_env, import_var_ctx, import_name_ctx, import_type_env) =
        Declaration.get_imported_name_env import_decl_l in
      let type_ctx =
        {
          (Type_ctx.build_type_ctx ()) with
          var_ctx= import_var_ctx;
          name_ctx= import_name_ctx;
          type_env= import_type_env;
        } in
      let (type_ctx, ty) = Type_checker.typing_expr type_ctx expr in
      Util.Debug.print_debug
        ("Type checking of " ^ Syntax.string_of_term expr ^ " provides "
       ^ Types.string_of_typ ty);
      let store =
        Interpreter.normalize_term_env ~val_env:import_val_env
          Type_ctx.empty_cons_ctx [] in
      ((expr, store), ty, Type_ctx.get_name_ctx type_ctx)
    with Type_checker.TypingError msg ->
      failwith ("Typing Error in the " ^ nbprog ^ " program:" ^ msg)

  let get_typed_namectx lexBuffer_signature =
    let signature_decl_l =
      parse_and_handle_error Parser.signature lexBuffer_signature in
    let (_, _, namectx, _) =
      Declaration.get_imported_name_env signature_decl_l in
    namectx

  let get_typed_ienv ?opponent_signature lexBuffer_implem lexBuffer_signature =
    try
      let implem_decl_l = parse_and_handle_error Parser.prog lexBuffer_implem in
      let signature_decl_l =
        parse_and_handle_error Parser.signature lexBuffer_signature in
      let import_decl_l =
        match opponent_signature with
        | None -> []
        | Some lexBuffer_imports ->
            parse_and_handle_error Parser.signature lexBuffer_imports in
      let (import_val_env, import_var_ctx, import_name_ctx, import_type_env) =
        Declaration.get_imported_name_env import_decl_l in
      let (comp_env, namectxO, cons_ctx) =
        Declaration.get_typed_comp_env ~import_var_ctx ~import_name_ctx
          ~import_type_env implem_decl_l signature_decl_l in
      let store =
        Interpreter.normalize_term_env ~val_env:import_val_env cons_ctx comp_env
      in
      let (ienv, namectxP) =
        Declaration.get_typed_val_env ~namectxO ~import_type_env store.valenv
          signature_decl_l in
      (* We should pass namectxO to get_typed_val_env so that ienv get the right image namectx*)
      (ienv, store, namectxP, namectxO)
    with Type_checker.TypingError msg -> failwith ("Typing Error: " ^ msg)
end

module MakeCompSymbolic (BranchMonad : Util.Monad.BRANCH) = struct
  include MakeCompBase (BranchMonad)
  module EvalMonad = Util.Monad.Result

  let normalize_opconf opconf =
    let open EvalMonad in
    match Interpreter.normalize_opconf opconf with
    | _ :: _ as res -> List.map (fun x -> Continue x) res
    | [] -> [ PropDiverges ]
end

module MakeCompConcrete (BranchMonad : Util.Monad.BRANCH) = struct
  include MakeCompBase (BranchMonad)
  module EvalMonad = Util.Monad.SingleResult

  let normalize_opconf opconf =
    let open EvalMonad in
    match Interpreter.normalize_opconf opconf with
    | res :: [] -> Continue res
    | [] -> PropDiverges
    | _ -> failwith "Non-determinism in concrete interpreter"
end

module WithAValBase (BranchMonad : Util.Monad.BRANCH) = struct
  type eval_context = Syntax.eval_context [@@deriving to_yojson]

  let pp_eval_context = Syntax.pp_eval_context
  let string_of_eval_context = Syntax.string_of_eval_context
  let empty_eval_context = Syntax.empty_eval_context
  let rename_eval_context = Syntax.rename_eval_context

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

  let typename_subst tvar_l tname_l =
    Util.Pmap.list_to_pmap
      (List.map2 (fun tvar tname -> (tvar, Types.TName tname)) tvar_l tname_l)

  let typename_ctx tname_l =
    List.fold_left
      (fun namectx tname ->
        snd (Namectx.Namectx.add_fresh namectx tname Types.TypeUniverse))
      Namectx.Namectx.empty tname_l

  let tnames_ienv = Ienv.tnames_ienv
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

  type normal_form_term =
    (Syntax.value, eval_context, Names.name, unit) Nf.nf_term

  let refold_nf_term = Syntax.refold_nf_term
  let get_nf_term = Syntax.get_nf_term
end

(* The RefML equations of {!Lang.Language.WITHAVAL_INOUT}, shared by the
   implementations below *)
module type WITHAVAL_INOUT_REFML =
  Lang.Language.WITHAVAL_INOUT
    with type term = Syntax.term
     and type value = Syntax.value
     and type negative_val = Syntax.negative_val
     and type typ = Types.typ
     and type negative_type = Types.negative_type
     and type Store.label = Syntax.label
     and type Store.Storectx.t = Store.Storectx.t
     and type Namectx.t = Namectx.Namectx.t
     and type Renaming.t = Renaming.Renaming.t
     and type IEnv.t = Ienv.IEnv.t
     and module Names = Names
     and type ('value, 'ectx, 'fname, 'cname) Nf.nf_term =
      ('value, 'ectx, 'fname, 'cname) Nf.nf_term
     and type AVal.abstract_val = Nup.nup

module WithAValSymbolic (BranchMonad : Util.Monad.BRANCH) :
  WITHAVAL_INOUT_REFML
    with module EvalMonad = Util.Monad.Result
     and module Store.BranchMonad = BranchMonad = struct
  include MakeCompSymbolic (BranchMonad)
  include WithAValBase (BranchMonad)

  module AVal :
    Lang.Abstract_val.AVAL
      with type name = Names.name
       and type interactive_env = Ienv.IEnv.t
       and type label = Syntax.label
       and type name_ctx = Namectx.t
       and type negative_type = Types.negative_type
       and type negative_val = Syntax.negative_val
       and type renaming = Renaming.t
       and type store_ctx = Store.Storectx.t
       and type store = Store.store
       and type typ = Types.typ
       and type value = Syntax.value
       and type abstract_val = Nup.nup
       and module BranchMonad = BranchMonad =
    Nup.Make (BranchMonad) (Nup.MakeGenerateSymbolicValue (BranchMonad))
end

module WithAValConcrete (BranchMonad : Util.Monad.BRANCH) :
  WITHAVAL_INOUT_REFML
    with module EvalMonad = Util.Monad.SingleResult
     and module Store.BranchMonad = BranchMonad = struct
  include MakeCompConcrete (BranchMonad)
  include WithAValBase (BranchMonad)

  module AVal :
    Lang.Abstract_val.AVAL
      with type name = Names.name
       and type interactive_env = Ienv.IEnv.t
       and type label = Syntax.label
       and type name_ctx = Namectx.t
       and type negative_type = Types.negative_type
       and type negative_val = Syntax.negative_val
       and type renaming = Renaming.t
       and type store_ctx = Store.Storectx.t
       and type store = Store.store
       and type typ = Types.typ
       and type value = Syntax.value
       and type abstract_val = Nup.nup
       and module BranchMonad = BranchMonad =
    Nup.Make (BranchMonad) (Nup.MakeGenerateConcreteValue (BranchMonad))
end
