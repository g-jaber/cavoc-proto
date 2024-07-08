type name_ctx = (Names.name, Types.typ) Util.Pmap.pmap
type var_ctx = (Syntax.id, Types.typ) Util.Pmap.pmap
type loc_ctx = (Syntax.loc, Types.typ) Util.Pmap.pmap
type cons_ctx = (Syntax.constructor, Types.typ) Util.Pmap.pmap

type type_ctx = {
  var_ctx: var_ctx;
  loc_ctx: loc_ctx;
  name_ctx: name_ctx;
  cons_ctx: cons_ctx;
  type_subst: Types.type_subst;
}

val get_var_ctx : type_ctx -> var_ctx
val get_name_ctx : type_ctx -> name_ctx
val get_loc_ctx : type_ctx -> loc_ctx
val get_type_subst : type_ctx -> Types.type_subst
val subst_vctx : Syntax.id -> Types.typ -> var_ctx -> var_ctx
val lsubst_type : Types.type_subst -> Types.typ -> Types.typ
val lsubst_vctx : Types.type_subst -> var_ctx -> var_ctx
val string_of_var_ctx : var_ctx -> string
val string_of_loc_ctx : loc_ctx -> string
val string_of_name_ctx : name_ctx -> string
val pp_name_ctx : Format.formatter -> name_ctx -> unit
val string_of_cons_ctx : cons_ctx -> string
val empty_name_ctx : name_ctx
val empty_var_ctx : var_ctx
val empty_loc_ctx : loc_ctx
val empty_cons_ctx : cons_ctx
val extend_type_subst : type_ctx -> Syntax.id -> Types.typ -> type_ctx
val update_type_subst : type_ctx -> Types.type_subst -> type_ctx
val extend_var_ctx : type_ctx -> Syntax.id -> Types.typ -> type_ctx
