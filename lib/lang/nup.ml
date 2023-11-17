module type NUP = sig
  (* to be instantiated *)
  type name
  type value
  type typ
  type name_ctx = (name, typ) Util.Pmap.pmap
  type val_env = (name, value) Util.Pmap.pmap
  (* *)

  type nup

  val string_of_nup : nup -> string
  val names_of_nup : nup -> name list

  (* type_check_nup takes Proponent's point of view when checking
     if nup is of typed ty within namectxP and namectxO the first and second name_ctx
      respectively for Proponent and Opponent names*)
  val type_check_nup : name_ctx -> name_ctx -> typ -> nup -> name_ctx option

  module M:Util.Monad.BRANCH
  val generate_nup : name_ctx -> typ -> (nup * name_ctx) M.m

  val unify_nup :
    name Util.Namespan.namespan ->
    nup ->
    nup ->
    name Util.Namespan.namespan option

  val abstract_val : value -> typ -> nup * val_env * name_ctx
  val subst_names_of_nup : val_env -> nup -> value
end
