module type NUP = sig
  (* to be instantiated *)
  type name
  type value
  type typ
  type typevar
  (* *)

  type nup

  type negative_type
  val get_negative_type : typ -> negative_type option
  val string_of_negative_type : negative_type -> string
  type name_ctx = (name, negative_type) Util.Pmap.pmap
  type val_env = (name, value) Util.Pmap.pmap

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

  val abstracting_value : value -> typ -> nup * val_env * name_ctx
  val subst_names : val_env -> nup -> value

  val get_input_type : negative_type -> typevar list * typ
  val get_output_type : negative_type -> typ
end
