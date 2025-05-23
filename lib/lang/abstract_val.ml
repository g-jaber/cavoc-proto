module type AVAL = sig
  (*To be instantiated*)
  type name
  (* labels are elements of domain of stores, 
     like locations or constructors*)
  type label
  type value
    (* The values filling the holes of abstracted values are negative values *)
  type negative_val
  type typ
    (* The names appearing in abstracted values are types by negative types *)
  type negative_type
  type name_ctx = (name, negative_type) Util.Pmap.pmap
  type store_ctx
    (* Interactive environments γ are partial maps from names to interactive values*)
  type interactive_env = (name, negative_val) Util.Pmap.pmap
  (* *)

  (* Abstracted values correspond to the observable part of a value.
        They are also called ultimate patterns.
  *)
  type abstract_val

  val pp_abstract_val : Format.formatter -> abstract_val -> unit
  val string_of_abstract_val : abstract_val -> string
  val names_of_abstract_val : abstract_val -> name list
  val labels_of_abstract_val : abstract_val -> label list

  (* The typed focusing process implemented by abstracting_value
     decomposes typed values (V,τ) into:
      - an abstract value A for the observable part,
      - a typed interactive environment (γ,Γ) for the negative part.
    The type τ is needed to guide this abstracting process for polymorphic languages. *)
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

  module M : Util.Monad.BRANCH

    (* From the interactive name context Γ_P and a type τ,
     we generate all the possible pairs (A,Δ) such that
     Γ_P;_ ⊢ A : τ ▷ Δ
     Freshness of names that appear in Δ is guaranteed by a gensym, so that we do not need to provide Γ_O. *)
  val generate_abstract_val : store_ctx -> name_ctx -> typ -> (abstract_val * name_ctx) M.m

  val unify_abstract_val :
    name Util.Namespan.namespan ->
    abstract_val ->
    abstract_val ->
    name Util.Namespan.namespan option
end