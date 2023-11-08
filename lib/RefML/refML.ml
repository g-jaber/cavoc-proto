module RefML : Lang.Cps.LANG = struct
  type computation = Syntax.exprML

  let string_of_computation = Syntax.string_of_exprML

  let get_typed_computation nbprog inBuffer =
    let lineBuffer = Lexing.from_channel inBuffer in
    try
      let expr = Parser.fullexpr Lexer.token lineBuffer in
      let ty = Type_checker.typing_full Util.Pmap.empty expr in
      let cn = Syntax.fresh_cname () in
      (Syntax.Named (cn, expr), Util.Pmap.singleton (cn, Types.TNeg ty))
    with
    | Lexer.SyntaxError msg ->
        failwith ("Parsing Error in the " ^ nbprog ^ " program:" ^ msg)
    | Parser.Error ->
        failwith
          ("Syntax Error in the " ^ nbprog ^ " program:" ^ " at position "
          ^ string_of_int (Lexing.lexeme_start lineBuffer))
    | Type_checker.TypingError msg ->
        failwith ("Typing Error in the " ^ nbprog ^ " program:" ^ msg)

  type interactive_type = Types.typeML

  let neg_type = Types.neg_type

  type name = Syntax.name

  let string_of_name = Syntax.string_of_name

  type cont_name = Syntax.id

  let string_of_cont_name cn = cn
  let get_cont_name = function Syntax.CName cn -> Some cn | _ -> None
  let inj_cont_name id = Syntax.CName id

  type name_type_ctx = Type_ctx.name_ctx

  let empty_name_type_ctx = Type_ctx.empty_name_ctx
  let string_of_name_type_ctx = Type_ctx.string_of_name_ctx

  type resources = Syntax.val_env * Heap.heap

  let string_of_resources (valenv, heap) =
    Syntax.string_of_val_env valenv ^ "| " ^ Heap.string_of_heap heap

  let empty_resources = (Syntax.empty_val_env, Heap.emptyheap)

  (* We trick the system by keeping the full val_env as type ctx *)
  type resources_type_ctx = Syntax.val_env * Type_ctx.loc_ctx

  let empty_resources_type_ctx = (Syntax.empty_val_env, Type_ctx.empty_loc_ctx)

  let string_of_resources_type_ctx (valenv, loc_ctx) =
    Syntax.string_of_val_env valenv ^ "|" ^ Type_ctx.string_of_loc_ctx loc_ctx

  let resources_type_ctx_of_resources (valenv, loc_ctx) =
    (valenv, Heap.loc_ctx_of_heap loc_ctx)

  let generate_resources (valenv, loc_ctx) =
    List.map (fun heap -> (valenv, heap)) (Heap.generate_heaps loc_ctx)

  type opconf = computation * resources
  type interactive_val = Syntax.exprML
  type interactive_env = Focusing.interactive_env

  let empty_ienv = Focusing.empty_ienv
  let singleton_ienv = Focusing.singleton_ienv
  let lookup_ienv = Focusing.lookup_ienv
  let concat_ienv = Focusing.concat_ienv
  let string_of_ienv = Focusing.string_of_interactive_env

  let get_typed_ienv inBuffer_implem inBuffer_signature =
    let lexer_implem = Lexing.from_channel inBuffer_implem in
    let lexer_signature = Lexing.from_channel inBuffer_signature in
    try
      let implem_decl_l = Parser.prog Lexer.token lexer_implem in
      let signature_decl_l = Parser.signature Lexer.token lexer_signature in
      let (comp_env, _) = Declaration.get_typed_comp_env implem_decl_l in
      let (val_env, heap) = Interpreter.compute_valenv comp_env in
      let (ienv, name_type_ctx) =
        Declaration.get_typed_int_env val_env signature_decl_l in
      (ienv, (val_env, heap), name_type_ctx)
    with
    | Lexer.SyntaxError msg -> failwith ("Parsing Error: " ^ msg)
    | Parser.Error ->
        failwith
          ("Syntax Error: " ^ " at position "
          ^ string_of_int (Lexing.lexeme_start lexer_implem))
        (* Need to get in which file the Parser.Error is *)
    | Type_checker.TypingError msg -> failwith ("Typing Error: " ^ msg)

  type nup = Focusing.nup

  let string_of_nup = Focusing.string_of_nup
  let generate_nup = Focusing.generate_nup
  let names_of_nup = Focusing.names_of_nup
  let type_check_nup = Focusing.type_check_nup

  let compute_nf (expr, (valenv, heap)) =
    match Interpreter.compute_nf (expr, valenv, heap) with
    | None -> None
    | Some (expr', valenv', heap') -> Some (expr', (valenv', heap'))

  let decompose_nf (expr, (valenv, heap)) =
    Focusing.decompose_nf (expr, valenv, heap)

  let val_composition = Focusing.val_composition
  let abstract_ival = Focusing.abstract_val
  let unify_nup = Focusing.unify_nup
end
