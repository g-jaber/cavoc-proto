module Names :
  Lang.Names.CONT_NAMES
    with type name = Names.name
     and type cont_name = Names.cont_name = struct
  include Names
end

module Typed :
  Lang.Language.TYPED
    with type typ = Types.typ
     and type negative_type = Types.negative_type
     and module Name = Names = struct
  module Name = Names

  type typ = Types.typ

  let string_of_type = Types.string_of_typ
  let pp_type = Types.pp_typ

  type negative_type = Types.negative_type

  let string_of_negative_type = Types.string_of_negative_type
  let pp_negative_type = Types.pp_negative_type
  let get_negative_type = Types.get_negative_type

  type name_ctx = (Name.name, negative_type) Util.Pmap.pmap

  let empty_name_ctx = Util.Pmap.empty
  let concat_name_ctx = Util.Pmap.concat
  let get_names_from_name_ctx = Util.Pmap.dom

  let string_of_name_ctx =
    Util.Pmap.string_of_pmap "[]" "::" Names.string_of_name
      Types.string_of_negative_type

  let pp_name_ctx = Type_ctx.pp_name_ctx
end

module MakeStore (M : Util.Monad.BRANCH) :
  Lang.Language.STORE
    with type store = Store.store
     and type label = Syntax.label
     and type store_ctx = Store.store_ctx
     and module M = M = struct
  include Store_gen.Make (M)
end

module MakeComp (M : Util.Monad.BRANCH) :
  Lang.Language.COMP
    with type term = Syntax.term
     and type value = Syntax.value
     and type negative_val = Syntax.negative_val
     and type typ = Types.typ
     and type negative_type = Types.negative_type
     and type Store.label = Syntax.label
     and type Store.store_ctx = Store.store_ctx
     and module Name = Names
     and module Store.M = M = struct
  include Syntax
  include Typed
  module Store = MakeStore (M)

  type opconf = Interpreter.opconf

  let normalize_opconf = Interpreter.normalize_opconf

  let get_typed_term nbprog inBuffer =
    let lineBuffer = Lexing.from_channel inBuffer in
    try
      let term = Parser.fullexpr Lexer.token lineBuffer in
      let ty = Type_checker.typing_full Util.Pmap.empty term in
      (term, ty, Util.Pmap.empty)
      (*TODO: The typing of Opponent names is missing, this should be corrected*)
    with
    | Lexer.SyntaxError msg ->
        failwith ("Parsing Error in the " ^ nbprog ^ " program:" ^ msg)
    | Parser.Error ->
        failwith
          ("Syntax Error in the " ^ nbprog ^ " program:" ^ " at position "
          ^ string_of_int (Lexing.lexeme_start lineBuffer))
    | Type_checker.TypingError msg ->
        failwith ("Typing Error in the " ^ nbprog ^ " program:" ^ msg)

  let get_typed_ienv inBuffer_implem inBuffer_signature =
    let lexer_implem = Lexing.from_channel inBuffer_implem in
    let lexer_signature = Lexing.from_channel inBuffer_signature in
    try
      let implem_decl_l = Parser.prog Lexer.token lexer_implem in
      let signature_decl_l = Parser.signature Lexer.token lexer_signature in
      let (comp_env, namectxO, cons_ctx) =
        Declaration.get_typed_comp_env implem_decl_l in
      let namectxO' = Util.Pmap.filter_map_im Types.get_negative_type namectxO in
      let (val_assign, heap, cons_ctx') =
        Interpreter.normalize_term_env cons_ctx comp_env in
      let (val_env, namectxP) =
        Declaration.get_typed_val_env val_assign signature_decl_l in
      let int_env = Util.Pmap.filter_map_im Syntax.filter_negative_val val_env in
      let namectxP' = Util.Pmap.filter_map_im Types.get_negative_type namectxP in
      (int_env, (val_assign, heap, cons_ctx'), namectxP', namectxO')
    with
    | Lexer.SyntaxError msg -> failwith ("Parsing Error: " ^ msg)
    | Parser.Error ->
        failwith
          ("Syntax Error: " ^ " at position "
          ^ string_of_int (Lexing.lexeme_start lexer_implem))
        (* Need to get in which file the Parser.Error is *)
    | Type_checker.TypingError msg -> failwith ("Typing Error: " ^ msg)
end

module WithAVal (M : Util.Monad.BRANCH) : Lang.Language.WITHAVAL_INOUT = struct
  include MakeComp (M)

  type eval_context = Syntax.eval_context

  let string_of_eval_context = Syntax.string_of_eval_context

  type typevar = Types.typevar
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

  module Nf_gen = Nf.Make (M)

  module Nf = struct
    include Nf
    module M = M

    let abstract_nf_term_m = Nf_gen.abstract_nf_term_m
  end

  let type_annotating_val = Nf.type_annotating_val
  let type_annotating_ectx = Nf.type_annotating_ectx
  let type_check_nf_term = Nf.type_check_nf_term
  let generate_nf_term_call = Nf_gen.generate_nf_term_call
  let generate_nf_term_ret = Nf_gen.generate_nf_term_ret

  type normal_form_term = (value, eval_context, Name.name, unit) Nf.nf_term

  let refold_nf_term = Syntax.refold_nf_term
  let get_nf_term = Syntax.get_nf_term

  (*
  let generate_nf_skeleton = Nf_gen.generate_nf_skeleton
  let fill_nf_skeleton = Nf_gen.fill_nf_skeleton*)

  module AVal :
    Lang.Abstract_val.AVAL
      with type name = Name.name
       and type value = Syntax.value
       and type negative_val = Syntax.negative_val
       and type typ = Types.typ
       and type negative_type = Types.negative_type
       and type label = Syntax.label
       and type store_ctx = Store.store_ctx
       and module M = M =
    Nup.Make (M)
end
