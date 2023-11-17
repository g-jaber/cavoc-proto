module WithNup (M:Util.Monad.BRANCH) : Lang.Cps.WITHNUP = struct
  type name = Syntax.name

  let string_of_name = Syntax.string_of_name
  let is_callable = Syntax.is_callable

  type cont_name = Syntax.id

  let string_of_cont_name cn = cn
  let get_cont_name = Syntax.cname_to_id
  let inj_cont_name = Syntax.cname_of_id
  let fresh_cname = Syntax.fresh_cname

  type term = Syntax.exprML

  let string_of_term = Syntax.string_of_exprML

  type value = Syntax.valML

  let string_of_value = Syntax.string_of_value

  type val_env = (name, value) Util.Pmap.pmap
  type eval_ctx = Syntax.eval_context

  let string_of_eval_ctx = Syntax.string_of_eval_context
  let fill_hole = Syntax.fill_hole
  let apply_value = Syntax.apply_value
  let get_callback = Syntax.get_callback
  let get_value = Syntax.get_value
  let get_raise = Syntax.get_raise
  let exception_type = Types.TExn

  (*let fresh_cname = Syntax.fresh_cname*)
  let is_error = Syntax.is_error

  type typ = Types.typeML
  type typevar = Types.typevar
  type typename = Types.id

  let string_of_type = Types.string_of_typeML
  let string_of_typename id = id

  let generate_typename_subst tvar_l =
    let aux tvar =
      let tname = Types.fresh_typename () in
      (tname, (tvar, Types.TName tname)) in
    let (tname_l, type_subst_l) = List.split @@ List.map aux tvar_l in
    (tname_l, Util.Pmap.list_to_pmap type_subst_l)

  let apply_type_subst = Types.apply_type_subst

  type name_ctx = (name, typ) Util.Pmap.pmap

  let get_input_type = function
    | Types.TArrow (ty1, _) -> ([], ty1)
    | Types.TForall (tvar_l, TArrow (ty1, _)) -> (tvar_l, ty1)
    | ty ->
        failwith @@ "Error: the type " ^ Types.string_of_typeML ty
        ^ "is not a negative type. Please report."

  let get_output_type = function
    | Types.TArrow (_, ty2) -> ty2
    | Types.TForall (_, TArrow (_, ty2)) -> ty2
    | ty ->
        failwith @@ "Error: the type " ^ Types.string_of_typeML ty
        ^ "is not a negative type. Please report."
     
  module Memory = struct
    type memory = Syntax.val_env * Heap.heap

    let string_of_memory (valenv, heap) =
      Syntax.string_of_val_env valenv ^ "| " ^ Heap.string_of_heap heap

    let empty_memory = (Syntax.empty_val_env, Heap.emptyheap)

    (* We trick the system by keeping the full val_env as type ctx *)
    type memory_type_ctx = Syntax.val_env * Type_ctx.loc_ctx

    let empty_memory_type_ctx = (Syntax.empty_val_env, Type_ctx.empty_loc_ctx)

    let string_of_memory_type_ctx (valenv, loc_ctx) =
      Syntax.string_of_val_env valenv ^ "|" ^ Type_ctx.string_of_loc_ctx loc_ctx

    let infer_type_memory (valenv, loc_ctx) =
      (valenv, Heap.loc_ctx_of_heap loc_ctx)

    module M = M
    let generate_memory (valenv, loc_ctx) =
      M.para_list @@ List.map (fun heap -> (valenv, heap)) (Heap.generate_heaps loc_ctx)
  end

  type opconf = term * Memory.memory

  let compute_nf (term, (val_env, heap)) =
    match Interpreter.compute_nf (term, val_env, heap) with
    | None -> None
    | Some (nf', val_env', heap') -> Some (nf', (val_env', heap'))

  let get_typed_term nbprog inBuffer =
    let lineBuffer = Lexing.from_channel inBuffer in
    try
      let expr = Parser.fullexpr Lexer.token lineBuffer in
      let ty = Type_checker.typing_full Util.Pmap.empty expr in
      (expr, ty, Util.Pmap.empty)
      (*TODO: To be corrected*)
    with
    | Lexer.SyntaxError msg ->
        failwith ("Parsing Error in the " ^ nbprog ^ " program:" ^ msg)
    | Parser.Error ->
        failwith
          ("Syntax Error in the " ^ nbprog ^ " program:" ^ " at position "
          ^ string_of_int (Lexing.lexeme_start lineBuffer))
    | Type_checker.TypingError msg ->
        failwith ("Typing Error in the " ^ nbprog ^ " program:" ^ msg)

  let get_typed_val_env inBuffer_implem inBuffer_signature =
    let lexer_implem = Lexing.from_channel inBuffer_implem in
    let lexer_signature = Lexing.from_channel inBuffer_signature in
    try
      let implem_decl_l = Parser.prog Lexer.token lexer_implem in
      let signature_decl_l = Parser.signature Lexer.token lexer_signature in
      let (comp_env, type_ctx) = Declaration.get_typed_comp_env implem_decl_l in
      let (val_assign, heap) = Interpreter.compute_val_env comp_env in
      let (val_env, name_ctxP) =
        Declaration.get_typed_val_env val_assign signature_decl_l in
      Util.Debug.print_debug @@ "The name context for Proponent is "
      ^ Type_ctx.string_of_name_ctx name_ctxP;
      (val_env, (val_assign, heap), name_ctxP, Type_ctx.get_name_ctx type_ctx)
    with
    | Lexer.SyntaxError msg -> failwith ("Parsing Error: " ^ msg)
    | Parser.Error ->
        failwith
          ("Syntax Error: " ^ " at position "
          ^ string_of_int (Lexing.lexeme_start lexer_implem))
        (* Need to get in which file the Parser.Error is *)
    | Type_checker.TypingError msg -> failwith ("Typing Error: " ^ msg)

  module Nup :
    Lang.Nup.NUP
      with type name = Syntax.name
       and type value = Syntax.valML
       and type typ = typ 
       and module M = M =
    Nup.Make(M)
end

module RefML (M:Util.Monad.BRANCH) : Lang.Interactive.LANG = Lang.Cps.Make (WithNup(M))
