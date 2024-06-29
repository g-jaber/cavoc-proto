module type INT = sig
  (* To be instantiated *)
  module IntLang : Lang.Interactive.LANG
  module Name : Lang.Names.CONT_NAMES with type name = IntLang.Name.name

  module Actions :
    Actions.ACTIONS
      with type Moves.name = IntLang.Name.name
       and type Moves.kdata = IntLang.abstract_normal_form
  (* *)

  type type_ctx

  val init_type_ctx :
    IntLang.Store.store_ctx -> IntLang.name_ctx -> IntLang.name_ctx -> type_ctx

  val string_of_type_ctx : type_ctx -> string
  val generate__moves : type_ctx -> (Actions.Moves.move * type_ctx) IntLang.M.m
  val check_input_move : type_ctx -> Actions.Moves.move -> type_ctx option
end
