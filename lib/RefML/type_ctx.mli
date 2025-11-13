type var_ctx = (Syntax.id, Types.typ) Util.Pmap.pmap
type loc_ctx = (Syntax.loc, Types.typ) Util.Pmap.pmap

type cons_ctx = (Syntax.constructor, Types.typ) Util.Pmap.pmap [@@deriving to_yojson]

type type_ctx = {
  var_ctx: var_ctx;
  loc_ctx: loc_ctx;
  name_ctx: Namectx.Namectx.t;
  cons_ctx: cons_ctx;
  type_env: Types.type_env;
}

val get_var_ctx : type_ctx -> var_ctx
val get_name_ctx : type_ctx -> Namectx.Namectx.t
val get_loc_ctx : type_ctx -> loc_ctx
val get_type_env : type_ctx -> Types.type_env
val pp_var_ctx : Format.formatter -> var_ctx -> unit
val pp_loc_ctx : Format.formatter -> loc_ctx -> unit
val pp_cons_ctx : Format.formatter -> cons_ctx -> unit
val string_of_var_ctx : var_ctx -> string
val string_of_loc_ctx : loc_ctx -> string
val string_of_cons_ctx : cons_ctx -> string
val empty_var_ctx : var_ctx
val empty_loc_ctx : loc_ctx
val empty_cons_ctx : cons_ctx
val extend_var_ctx : type_ctx -> Syntax.id -> Types.typ -> type_ctx
val apply_type_subst : type_ctx -> Types.type_subst -> type_ctx

val build_type_ctx : Syntax.term -> type_ctx