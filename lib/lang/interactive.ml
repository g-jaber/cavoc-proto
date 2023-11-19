module type LANG = sig
  include Names.CONT_NAMES

  type computation

  val string_of_computation : computation -> string

  module M : Util.Monad.BRANCH
  module Memory : Language.MEMORY with module M = M

  type opconf = computation * Memory.memory

  (* compute_nf computes the normal form of an operational configuration,
     or None when we detect that the operational configuration diverges.*)
  val compute_nf : opconf -> opconf option

  (* we classify the interaction (like returning a value or performing a callbacks) using the type kind_interact *)
  type kind_interact

  val string_of_kind_interact : kind_interact -> string
  val name_of_kind_interact : kind_interact -> name option

  val is_equiv_kind_interact :
    name Util.Namespan.namespan -> kind_interact -> kind_interact -> bool

  (* Normal forms are either decomposed into their kind of interaction
      and a glue values on which the interaction happened,
      or into None when they corresponds to (uncatchable) error values. *)
  type glue_val
  type glue_type

  val decompose_nf : computation -> (kind_interact * glue_val) option

  (* Abstracted values correspond to the observable part of a value.
      They are also called ultimate patterns.
  *)
  type abstract_val

  val string_of_abstract_val : abstract_val -> string
  val names_of_abstract_val : abstract_val -> name list

  (* Interactive values are the negative part of values,
     that are not exchanged between players but rather interact on. *)
  type interactive_val

  (* Interactive types are used to type interactive values.
     Notice that they are not necessarily a subset of the types of the programming language. *)
  type interactive_type

  (* The function neg_type extract from an interactive type the type of the input arguments
     expected to interact over this type. *)
  val neg_type : interactive_type -> glue_type

  (*Interactive name contexts are typing contexts mapping names to interactive types.*)
  type name_type_ctx (*= (name, interactive_type) Util.Pmap.pmap*)

  val empty_name_type_ctx : name_type_ctx
  val concat_name_type_ctx : name_type_ctx -> name_type_ctx -> name_type_ctx
  val string_of_name_type_ctx : name_type_ctx -> string
  val get_names_from_name_type_ctx : name_type_ctx -> name list

  (* kind_interact_typing provide a way to type check an interact kind within an interactive name context. 
     It returns None if the interactive kind is not well-typed.*)
  val kind_interact_typing :
    kind_interact -> name_type_ctx -> interactive_type option


  val extract_kind_interact : name_type_ctx -> (kind_interact*interactive_type) list

  (* Interactive environments γ are partial maps from names to interactive values*)
  type interactive_env

  val empty_ienv : interactive_env
  val singleton_ienv : name * interactive_val -> interactive_env
  val list_to_ienv : (name * interactive_val) list -> interactive_env
  val trigger_ienv : interactive_env -> kind_interact -> interactive_val option
  val concat_ienv : interactive_env -> interactive_env -> interactive_env
  val string_of_interactive_env : interactive_env -> string

  (* The typed focusing process implemented by abstract_glue_val
      decomposes typed glue values into:
       - an abstract value for the observable part,
       - a typed interactive environment for the negative part. *)
  val abstract_glue_val :
    glue_val -> glue_type -> abstract_val * interactive_env * name_type_ctx

  (* The typing judgment of an abstracted value Γ_P;Γ_O ⊢ A : τ ▷ Δ
     produces the interactive name contexts Δ of fresh names introduced by A.
     it returns None when the type checking fails.
     The context Γ_P is used to retrieve the existing polymorphic names, and to check for freshness other names.
     The contexts Γ_O is used to check for freshness of names *)
  val type_check_abstract_val :
    name_type_ctx ->
    name_type_ctx ->
    glue_type ->
    abstract_val ->
    name_type_ctx option

  (* From the interactive name context Γ_P and a glue type τ,
     we generate all the possible pairs (A,Δ) such that
     Γ_P;_ ⊢ A : τ ▷ Δ
     Freshness of names that appear in Δ is guaranteed by a gensym, so that we do not need to provide Γ_O. *)
  val generate_abstract_val :
    name_type_ctx -> glue_type -> (abstract_val * name_type_ctx) M.m

  (* From an interactive environment γ, an interactive value I and an abstract value A,
      val_composition γ I A  built the computation I ★ A{γ} *)
  val val_composition :
    interactive_env -> interactive_val -> abstract_val -> computation

  val unify_abstract_val :
    name Util.Namespan.namespan ->
    abstract_val ->
    abstract_val ->
    name Util.Namespan.namespan option

  val get_typed_computation :
    string -> in_channel -> computation * name_type_ctx

  (* The function get_typed_ienv
     retrive a module declaration and its signature from the two in_channel taken as input.
     It evaluates the list of computation declarations
     into a list of value declarations together with the memory
     generated by this evaluation. *)
  val get_typed_ienv :
    in_channel ->
    in_channel ->
    interactive_env * Memory.memory * name_type_ctx * name_type_ctx
end
