module type LANG = sig
  type computation
  val string_of_computation : computation -> string

  type interactive_type
  val neg_type : interactive_type -> interactive_type

  type name
  val string_of_name : name -> string

  type name_type_ctx = (name, interactive_type) Util.Pmap.pmap
  val string_of_name_type_ctx : name_type_ctx -> string

  val get_typed_computation : string -> in_channel -> (computation*name_type_ctx)

  type resources
  val string_of_resources : resources -> string
  val empty_resources : resources

  type resources_type_ctx
  val empty_resources_type_ctx : resources_type_ctx
  val string_of_resources_type_ctx : resources_type_ctx -> string
  val resources_type_ctx_of_resources : resources -> resources_type_ctx
  val generate_resources : resources_type_ctx -> resources list

  type opconf = computation * resources

  type interactive_val
  type interactive_env
  val empty_ienv : interactive_env
  val singleton_ienv : (name*interactive_val) -> interactive_env
  val lookup_ienv : name -> interactive_env -> interactive_val option
  val concat_ienv : interactive_env -> interactive_env -> interactive_env
  val string_of_ienv : interactive_env -> string
  val get_typed_ienv : in_channel -> (interactive_env*name_type_ctx)

  type nup
  val string_of_nup : nup -> string
  val generate_nup : interactive_type -> (nup*name_type_ctx) list
  val names_of_nup : nup -> name list
  val type_check_nup : name_type_ctx -> interactive_type -> nup -> name_type_ctx option

  val compute_nf : opconf -> opconf option
  val decompose_nf : opconf -> name*interactive_val

  val val_composition : interactive_val -> nup -> computation

  val abstract_ival : interactive_val -> interactive_type -> nup * interactive_env * name_type_ctx
  val unify_nup : 
    name Util.Namespan.namespan ->
    nup -> nup -> name Util.Namespan.namespan option
end