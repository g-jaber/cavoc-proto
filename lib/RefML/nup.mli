type nup

val string_of_nup : nup -> string
val names_of_nup : nup -> Syntax.name list

val generate_nup :
  Type_ctx.name_ctx -> Types.typeML -> (nup * Type_ctx.name_ctx) list

(* type_check_nup takes Proponent's point of view when checking
   if nup is of typed ty within namectxP and namectxO the first and second name_ctx
    respectively for Proponent and Opponent names*)
val type_check_nup :
  Type_ctx.name_ctx ->
  Type_ctx.name_ctx ->
  Types.typeML ->
  nup ->
  Type_ctx.name_ctx option

val unify_nup :
  Syntax.name Util.Namespan.namespan ->
  nup ->
  nup ->
  Syntax.name Util.Namespan.namespan option

type val_env = (Syntax.name,Syntax.valML) Util.Pmap.pmap

val abstract_val :   Syntax.valML -> Types.typeML -> nup * val_env * Type_ctx.name_ctx

val incorporate_name : (nup*Syntax.name) -> nup

val subst_names_of_nup : val_env -> nup -> Syntax.valML