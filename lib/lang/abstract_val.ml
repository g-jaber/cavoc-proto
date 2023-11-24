module type AVAL = sig
  (*To be instantiated*)
  type name
  type value
  type negative_val
  type typ
  type negative_type
  type typevar
  type name_ctx = (name, negative_type) Util.Pmap.pmap
  type interactive_env = (name, negative_val) Util.Pmap.pmap
  (* *)

  (* Abstracted values correspond to the observable part of a value.
        They are also called ultimate patterns.
  *)
  type abstract_val

  val string_of_abstract_val : abstract_val -> string
  val names_of_abstract_val : abstract_val -> name list

  (* The names appearing in abstracted values are types by negative types *)

  (*Interactive name contexts are typing contexts mapping names
     to negative types.*)

  (**  TODO: This should be moved elswehere **)

  (* The values filling the holes of abstracted values are negative values *)

  (* Interactive environments γ are partial maps from names to interactive values*)

  (*val embed_value_env : (name,value) Util.Pmap.pmap -> interactive_env*)

  (* The typed focusing process implemented by abstract_value
     decomposes typed glue values into:
      - an abstract value for the observable part,
      - a typed interactive environment for the negative part. *)
  val abstracting_value :
    value -> typ -> abstract_val * interactive_env * name_ctx

  val subst_names : interactive_env -> abstract_val -> value

  (* The typing judgment of an abstracted value Γ_P;Γ_O ⊢ A : τ ▷ Δ
     produces the interactive name contexts Δ of fresh names introduced by A.
     it returns None when the type checking fails.
     The context Γ_P is used to retrieve the existing polymorphic names, and to check for freshness other names.
     The contexts Γ_O is used to check for freshness of names *)
  val type_check_abstract_val :
    name_ctx -> name_ctx -> typ -> abstract_val -> name_ctx option

  (* From the interactive name context Γ_P and a type τ,
     we generate all the possible pairs (A,Δ) such that
     Γ_P;_ ⊢ A : τ ▷ Δ
     Freshness of names that appear in Δ is guaranteed by a gensym, so that we do not need to provide Γ_O. *)
  module M : Util.Monad.BRANCH

  val generate_abstract_val : name_ctx -> typ -> (abstract_val * name_ctx) M.m

  val unify_abstract_val :
    name Util.Namespan.namespan ->
    abstract_val ->
    abstract_val ->
    name Util.Namespan.namespan option
end

module type AVAL_INOUT = sig
  include AVAL

  val get_input_type : negative_type -> typevar list * typ
  val get_output_type : negative_type -> typ
end

module type AVAL_NEG = sig
  include AVAL

  val negating_type : negative_type -> typ
end
