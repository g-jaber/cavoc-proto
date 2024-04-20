module Names :
  Lang.Names.CONT_NAMES
    with type name = Names.name
     and type cont_name = Names.cont_name = struct
  include Names
end

module Typed :
  Lang.Language.TYPED
    with type typ = Types.vtyp
     and type negative_type = Types.negative_type
     and module Name = Names = struct
  module Name = Names

  type typ = Types.vtyp

  let exception_type = failwith "exception_type not defined"

  let string_of_type = Types.string_of_vtyp

  type negative_type = Types.negative_type

  let string_of_negative_type = Types.string_of_negative_type
  let get_negative_type = Types.get_negative_type

  type name_ctx = (Name.name, negative_type) Util.Pmap.pmap

  let empty_name_ctx = Util.Pmap.empty
  let concat_name_ctx = Util.Pmap.concat
  let get_names_from_name_ctx = Util.Pmap.dom

  let string_of_name_ctx =
    Util.Pmap.string_of_pmap "[]" "::" Names.string_of_name
      Types.string_of_negative_type
end

module MakeStore  (M : Util.Monad.BRANCH) = struct
  type store = unit

  let string_of_store () = "()"
  let empty_store = ()

  type store_ctx = unit

  let empty_store_ctx = ()
  let string_of_store_ctx () = ""
  let concat_store_ctx () () = ()
  let infer_type_store () = ()

  (* update_store μ μ' is equal to μ[μ'] *)
  let update_store () () = ()
  let restrict () () = ()

  type label = Syntax.opsymbol

  let restrict_ctx () label_list = ()

  module M = M
  (* *)

  let generate_store () = M.return ()
end

module MakeComp (M : Util.Monad.BRANCH) :
  Lang.Language.COMP
    with type term = Syntax.computation
     and type value = Syntax.value
     and type negative_val = Syntax.negative_val
     and type typ = Types.vtyp
     and type negative_type = Types.negative_type
     and type Store.label = Syntax.label
     and type Store.store_ctx = unit
     and module Name = Names
     and module Store.M = M = struct
  include Syntax
  include Typed
  module Store = MakeStore (M)

  type opconf = computation*unit (*Interpreter.config*)
  type term = computation
  let string_of_term = string_of_computation

  let normalize_opconf (comp,()) = 
    match comp |> Interpreter.inj_config |> Interpreter.normalize_conf with
     | Some nf_conf -> Some (nf_conf.term,())
     | None -> None

  let get_typed_term nbprog inBuffer =
    let lineBuffer = Lexing.from_channel inBuffer in
    try
      let term = Parser.fullterm Lexer.token lineBuffer in
      let (TComp (ty,_)) = Type_checker.typing_full Util.Pmap.empty term in
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
      let (comp_env, namectxO, cons_ctx) = Declaration.get_typed_comp_env implem_decl_l in
      let namectxO' = Util.Pmap.filter_map_im Types.get_negative_type namectxO in
      (*let (val_assign, heap, cons_ctx') = Interpreter.normalize_term_env cons_ctx comp_env in *)
      let (val_assign, cons_ctx') = Interpreter.normalize_term_env cons_ctx comp_env in
      let (val_env, namectxP) =
        Declaration.get_typed_val_env val_assign signature_decl_l in
      let int_env = Util.Pmap.filter_map_im Syntax.filter_negative_val val_env in
      let namectxP' = Util.Pmap.filter_map_im Types.get_negative_type namectxP in
      (*(int_env, (val_assign, heap, cons_ctx'), namectxP', namectxO') *)
      (int_env, val_assign, cons_ctx', namectxP', namectxO')
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

  module AVal :
    Lang.Abstract_val.AVAL_INOUT
      with type name = Name.name
       and type value = Syntax.value
       and type negative_val = Syntax.negative_val
       and type vtyp = Types.vtyp
       and type ctyp = Types.ctyp
       and type negative_type = Types.negative_type
       and type typevar = Types.typevar
       and type name_ctx = name_ctx
       (*and type store_ctx = Store.store_ctx *)
       (*and type label = Syntax.label *)
       and module M = M =
    Nup.Make (M)
end
