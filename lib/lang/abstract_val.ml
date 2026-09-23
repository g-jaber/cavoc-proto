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
  type store
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
  (* A location of V already disclosed in µ is abstracted as ALocFree of its
     location name; one not yet disclosed is disclosed by the move, at the
     location name fresh for Σ, and abstracted as ALocBound. *)
  val abstracting_value :
    value -> name_ctx -> store -> typ -> abstract_val * interactive_env * store

  (* abstracting_store γ µ values extends values with the abstracted value
     of every disclosed location it does not cover, to a fixpoint since a
     stored value may disclose locations; their fresh names extend γ. *)
  val abstracting_store :
    interactive_env ->
    store ->
    abstract_val list ->
    abstract_val list * interactive_env * store

  (* subst_pnames γ µ τ A replaces the names of γ in A, read at the type τ, by
     their values and its location names by their locations in µ. *)
  val subst_pnames : interactive_env -> store -> typ -> abstract_val -> value

  (* concretize_store γ µ Σ' values allocates the locations of Σ' absent from
     µ, then writes the values of the locations of Σ' in µ. *)
  val concretize_store :
    interactive_env -> store -> store_ctx -> abstract_val list -> store

  (* rename A ρ instantiates the bound names of A along ρ : Δ → Γ+Δ. *)
  val rename : abstract_val -> renaming -> abstract_val

  (* The typing judgment Σ;Γ_P;Γ_O ⊢ A : τ ▷ Δ of an abstracted value, over both
     name contexts of the position, Δ being the fresh names introduced by A. *)
  (* A free polymorphic name is looked up at its type in Γ_P then in Γ_O; by
     its kind exactly one of them can hold it. *)
  (* The pair (Σ, Δ) the move has declared so far, given first, is returned
     extended with what A binds, a bound name having to be the next one; None
     when A is ill typed. *)
  val type_check_abstract_val :
    store_ctx ->
    name_ctx ->
    name_ctx ->
    name_ctx ->
    typ ->
    abstract_val ->
    (store_ctx * name_ctx) option

  module BranchMonad : Util.Monad.BRANCH

  (* From the interactive name context Γ_P and a type τ,
     we generate all the possible pairs (A,Δ) such that
     Γ_P;_ ⊢ A : τ ▷ Δ
     The names introduced by A are de Bruijn levels of the locally built Δ,
     given an ambient identity by the weakening Δ ↪ Γ_O + Δ that the machine
     computes, so that we do not need to provide Γ_O. *)
  (* The first two arguments are the pair (Σ, Δ) the move has declared so
     far, returned extended. *)
  val generate_abstract_val :
    store_ctx ->
    name_ctx ->
    name_ctx ->
    typ ->
    (abstract_val * (store_ctx * name_ctx)) BranchMonad.m

  (* Equivalence up to the discriminating power of the environment. *)
  val is_equiv_abstract_val : store -> store -> abstract_val -> abstract_val -> bool
end
