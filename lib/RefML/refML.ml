module RefML : Lang.Cps.LANG = struct
  type computation = Syntax.exprML
  let string_of_computation = Syntax.string_of_exprML 
  let get_typed_computation nbprog inBuffer = 
    let lineBuffer = Lexing.from_channel inBuffer in
    try let expr = Parser.fullexpr Lexer.token lineBuffer in
        let ty = Type_checker.typing_full Util.Pmap.empty expr in
        Syntax.init_term expr ty
    with
    | Lexer.SyntaxError msg -> failwith ("Parsing Error in the " ^ nbprog
                                        ^ " program:" ^ msg)
    | Parser.Error ->
      failwith ("Syntax Error in the " ^ nbprog ^ " program:"
                ^ " at position "
                ^ (string_of_int (Lexing.lexeme_start lineBuffer)))
    | Type_checker.TypingError msg -> 
      failwith ("Typing Error in the " ^ nbprog ^ " program:"
                 ^ msg)
  
  type interactive_type = Types.typeML
  let neg_type = Types.neg_type
  type name = Syntax.name
  let string_of_name = Syntax.string_of_name
  
  type cont_name = Syntax.id
  let string_of_cont_name cn = cn
  let get_cont_name = function
      | Syntax.CName cn -> Some cn
      | _ -> None

  let inj_cont_name id = Syntax.CName id

  
  type name_type_ctx = Syntax.name_ctx
  let string_of_name_type_ctx = Syntax.string_of_name_ctx
  type resources = Heap.heap
  let string_of_resources = Heap.string_of_heap
  let empty_resources = Heap.emptyheap

  type resources_type_ctx = Syntax.loc_ctx
  let empty_resources_type_ctx = Syntax.empty_loc_ctx
  let string_of_resources_type_ctx = Syntax.string_of_loc_ctx
  let resources_type_ctx_of_resources = Heap.loc_ctx_of_heap
  let generate_resources = Heap.generate_heaps

  type opconf = computation * resources
  type interactive_val = Syntax.exprML
  type interactive_env = Focusing.interactive_env
  let empty_ienv = Focusing.empty_ienv
  let singleton_ienv = Focusing.singleton_ienv
  let lookup_ienv = Focusing.lookup_ienv
  let concat_ienv = Focusing.concat_ienv
  let string_of_ienv = Focusing.string_of_interactive_env
  let get_typed_ienv inBuffer = 
    let lineBuffer = Lexing.from_channel inBuffer in
    try let implem_decl_l = Parser.prog Lexer.token lineBuffer in
        Declaration.get_ienv implem_decl_l
    with
    | Lexer.SyntaxError msg -> failwith ("Parsing Error: " ^ msg)
    | Parser.Error ->
      failwith ("Syntax Error: "
                ^ " at position "
                ^ (string_of_int (Lexing.lexeme_start lineBuffer)))
    | Type_checker.TypingError msg -> 
      failwith ("Typing Error: " ^ msg)


  type nup = Focusing.nup
  let string_of_nup = Focusing.string_of_nup
  let generate_nup = Focusing.generate_nup
  let names_of_nup = Focusing.names_of_nup
  let type_check_nup = Focusing.type_check_nup

  let compute_nf = Interpreter.compute_nf
  let decompose_nf = Focusing.decompose_nf

  let val_composition = Focusing.val_composition

  let abstract_ival = Focusing.abstract_val
  let unify_nup = Focusing.unify_nup
end