module Names :
  Lang.Names.CONT_NAMES
    with type name = Names.name
     and type cont_name = Names.cont_name = struct
  include Names
end

module Basic :
  Lang.Language.BASIC
    with type term = Syntax.term
     and type value = Syntax.value
     and type negative_val = Syntax.negative_val
     and module Name = Names = struct
  module Name = Names 
  (* *)
  include Syntax
end

module Typed :
  Lang.Language.TYPED
    with type term = Syntax.term
     and type value = Syntax.value
     and type negative_val = Syntax.negative_val
     and type typ = Types.typ
     and type negative_type = Types.negative_type
     and type typevar = Types.typevar
     and module Name = Names = struct
  include Basic

  type typ = Types.typ
  type typevar = Types.typevar
  type typename = Types.id

  let string_of_type = Types.string_of_typ
  let string_of_typename id = id

  type negative_type = Types.negative_type

  let string_of_negative_type = Types.string_of_negative_type
  let get_negative_type = Types.get_negative_type

  type name_ctx = (Name.name, negative_type) Util.Pmap.pmap

  let empty_name_ctx = Util.Pmap.empty
  let concat_name_ctx = Util.Pmap.concat
  let get_names_from_name_ctx = Util.Pmap.dom

  let string_of_name_ctx =
    Util.Pmap.string_of_pmap "ε" "::" Names.string_of_name
      Types.string_of_negative_type

  let exception_type = Types.TExn

  let generate_typename_subst tvar_l =
    let aux tvar =
      let tname = Types.fresh_typename () in
      (tname, (tvar, Types.TName tname)) in
    let (tname_l, type_subst_l) = List.split @@ List.map aux tvar_l in
    (tname_l, Util.Pmap.list_to_pmap type_subst_l)

  let apply_type_subst = Types.apply_type_subst
end

module Memory (M : Util.Monad.BRANCH) :
  Lang.Language.MEMORY with type memory = Interpreter.memory and module M = M =
struct
  type memory = Interpreter.memory

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
    M.para_list
    @@ List.map (fun heap -> (valenv, heap)) (Heap.generate_heaps loc_ctx)
end

module Comp (M : Util.Monad.BRANCH) :
  Lang.Language.COMP
    with type term = Syntax.term
     and type value = Syntax.value
     and type negative_val = Syntax.negative_val
     and type typ = Types.typ
     and type negative_type = Types.negative_type
     and type typevar = Types.typevar
     and module Name = Names
     and module Memory.M = M = struct
  include Typed
  module Memory = Memory (M)

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

  let get_typed_interactive_env inBuffer_implem inBuffer_signature =
    let lexer_implem = Lexing.from_channel inBuffer_implem in
    let lexer_signature = Lexing.from_channel inBuffer_signature in
    try
      let implem_decl_l = Parser.prog Lexer.token lexer_implem in
      let signature_decl_l = Parser.signature Lexer.token lexer_signature in
      let (comp_env, namectxO) = Declaration.get_typed_comp_env implem_decl_l in
      let namectxO' = Util.Pmap.filter_map_im Types.get_negative_type namectxO in
      let (val_assign, heap) = Interpreter.normalize_term_env comp_env in
      let (val_env, namectxP) =
        Declaration.get_typed_val_env val_assign signature_decl_l in
      let int_env = Util.Pmap.filter_map_im Syntax.filter_negative_val val_env in
      let namectxP' = Util.Pmap.filter_map_im Types.get_negative_type namectxP in
      (int_env, (val_assign, heap), namectxP', namectxO')
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
  include Comp (M)

  module AVal :
    Lang.Abstract_val.AVAL_INOUT
      with type name = Name.name
       and type value = Syntax.value
       and type negative_val = Syntax.negative_val
       and type typ = Types.typ
       and type typevar = Types.typevar
       and type negative_type = Types.negative_type
       and module M = M =
    Nup.Make (M)
end
