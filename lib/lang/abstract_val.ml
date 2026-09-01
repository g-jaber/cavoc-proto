module type AVAL = sig
  (*To be instantiated*)
  type name
  type renaming

  (* labels are elements of domain of stores, 
     like locations or constructors*)
  type label
  type value
  (* The values filling the holes of abstracted values are negative values *)

  type negative_val
  type typ
  (* The names appearing in abstracted values are types by negative types *)

  type negative_type
  type name_ctx
  type store_ctx
  (* Interactive environments γ are partial maps from names to interactive values*)

  type interactive_env
  (* *)

  (* Abstracted values correspond to the observable part of a value.
        They are also called ultimate patterns.
  *)
  type abstract_val [@@deriving to_yojson]

  val pp_abstract_val : Format.formatter -> abstract_val -> unit

  (* Like pp_abstract_val, with bound and free names displayed by the
     provided printers. *)
  val pp_abstract_val_in :
    pp_free_name:(Format.formatter -> name -> unit) ->
    pp_bound_name:(Format.formatter -> name -> unit) ->
    Format.formatter ->
    abstract_val ->
    unit

  val string_of_abstract_val : abstract_val -> string
  val names_of_abstract_val : abstract_val -> name list
  val labels_of_abstract_val : abstract_val -> label list

  (* fold_free_names_of_abstract_val f acc A folds f over the free names of A
     — the names reused from the ambient context — skipping the bound names,
     which are local to the move introducing A. *)
  val fold_free_names_of_abstract_val :
    ('a -> name -> 'a) -> 'a -> abstract_val -> 'a

  (* map_free_names_of_abstract_val f A renames the free names of A along f,
     leaving its bound names untouched. *)
  val map_free_names_of_abstract_val :
    (name -> name) -> abstract_val -> abstract_val

  (* The typed focusing process implemented by abstracting_value
     decomposes typed values (V,τ) into:
      - an abstract value A for the observable part,
      - a typed interactive environment γ for the negative part.
    The type τ is needed to guide this abstracting process for polymorphic languages. *)
  val abstracting_value :
    value -> name_ctx -> typ -> abstract_val * interactive_env

  val subst_pnames : interactive_env -> abstract_val -> value

  (* rename A ρ instantiates the bound names of A along ρ : Δ → Γ+Δ. *)
  val rename : abstract_val -> renaming -> abstract_val

  (* The typing judgment of an abstracted value Γ ⊢ A : τ ▷ Δ
     produces the interactive name contexts Δ of fresh names introduced by A.
     it returns None when the type checking fails.
     The context Γ_P is used to retrieve the existing polymorphic names, and to check for freshness other names.
     The contexts Γ_O is used to check for freshness of names *)
  val type_check_abstract_val :
    store_ctx -> name_ctx -> typ -> abstract_val * name_ctx -> bool

  module BranchMonad : Util.Monad.BRANCH

  (* From the interactive name context Γ_P and a type τ,
     we generate all the possible pairs (A,Δ) such that
     Γ_P;_ ⊢ A : τ ▷ Δ
     The names introduced by A are de Bruijn levels of the locally built Δ,
     given an ambient identity by the weakening Δ ↪ Γ_O + Δ that the machine
     computes, so that we do not need to provide Γ_O. *)
  val generate_abstract_val :
    store_ctx ->
    name_ctx ->
    typ ->
    (abstract_val * (store_ctx * name_ctx)) BranchMonad.m

  val unify_abstract_val :
    name Util.Namespan.namespan ->
    abstract_val ->
    abstract_val ->
    name Util.Namespan.namespan option
end
