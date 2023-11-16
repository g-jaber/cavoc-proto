type name_ctx = (Syntax.name, Types.typeML) Util.Pmap.pmap
type var_ctx = (Syntax.id, Types.typeML) Util.Pmap.pmap
type loc_ctx = (Syntax.loc, Types.typeML) Util.Pmap.pmap
type type_ctx =
  {var_ctx:var_ctx; loc_ctx:loc_ctx; name_ctx:name_ctx; exn_ctx: Syntax.constructor list; type_subst: Types.type_subst}

val get_var_ctx : type_ctx -> var_ctx
val get_name_ctx : type_ctx -> name_ctx
val get_loc_ctx : type_ctx -> loc_ctx
val get_type_subst : type_ctx -> Types.type_subst

val subst_vctx : Syntax.id -> Types.typeML -> var_ctx -> var_ctx
val lsubst_type : Types.type_subst -> Types.typeML -> Types.typeML
val lsubst_vctx : Types.type_subst -> var_ctx -> var_ctx
val string_of_var_ctx : var_ctx -> string
val string_of_loc_ctx : loc_ctx -> string
val string_of_name_ctx : name_ctx -> string
val empty_name_ctx : name_ctx
val empty_var_ctx : var_ctx
val empty_loc_ctx : loc_ctx

val extend_type_subst : type_ctx -> Syntax.id -> Types.typeML -> type_ctx
val update_type_subst : type_ctx -> Types.type_subst -> type_ctx
val extend_var_ctx : type_ctx -> Syntax.id -> Types.typeML -> type_ctx