module type TYPED = sig
  module Name : Names.CONT_NAMES

  type typ

  val string_of_type : typ -> string
  val pp_type : Format.formatter -> typ -> unit

  type negative_type

  val get_negative_type : typ -> negative_type option
  val string_of_negative_type : negative_type -> string
  val pp_negative_type : Format.formatter -> negative_type -> unit

  type name_ctx = (Name.name, negative_type) Util.Pmap.pmap

  val empty_name_ctx : name_ctx
  val concat_name_ctx : name_ctx -> name_ctx -> name_ctx
  val string_of_name_ctx : name_ctx -> string
  val pp_name_ctx : Format.formatter -> name_ctx -> unit
  val get_names_from_name_ctx : name_ctx -> Name.name list
end

module type STORE = sig
  type store

  val string_of_store : store -> string
  val pp_store : Format.formatter -> store -> unit
  val empty_store : store

  type store_ctx

  val empty_store_ctx : store_ctx
  val string_of_store_ctx : store_ctx -> string
  val pp_store_ctx : Format.formatter -> store_ctx -> unit
  val concat_store_ctx : store_ctx -> store_ctx -> store_ctx
  val infer_type_store : store -> store_ctx

  (* update_store μ μ' is equal to μ[μ'] *)
  val update_store : store -> store -> store
  val restrict : store_ctx -> store -> store

  type label

  val restrict_ctx : store_ctx -> label list -> store_ctx

  module M : Util.Monad.BRANCH
  (* *)

  val generate_store : store_ctx -> store M.m
end

module type COMP = sig
  include TYPED
  module Store : STORE

  type term

  val pp_term : Format.formatter -> term -> unit
  val string_of_term : term -> string

  type value

  val pp_value : Format.formatter -> value -> unit
  val string_of_value : value -> string

  type negative_val

  val pp_negative_val : Format.formatter -> negative_val -> unit
  val string_of_negative_val : negative_val -> string
  val filter_negative_val : value -> negative_val option

  type interactive_env = (Name.name, negative_val) Util.Pmap.pmap

  val empty_ienv : interactive_env
  val concat_ienv : interactive_env -> interactive_env -> interactive_env
  val string_of_ienv : interactive_env -> string
  val pp_ienv : Format.formatter -> interactive_env -> unit

  type opconf = term * Store.store

  (* Evaluation of programs may produces:
     - divergence
     - uncatchable error
     - raised exception that has not (yet) been caught
     - values
     - callbacks to function provided by another module
  *)

  val normalize_opconf : opconf -> opconf option
  val get_typed_term : string -> Lexing.lexbuf -> term * typ * name_ctx

  (* We retrive a module declaration and its signature from the two in_channel taken as input.
     We evaluate the list of computation declarations into a list of value declarations together with the store
     generated by this evaluation.
     We return two typing contexts, respectively for the names in the domain of the interactive env,
     and the one contains in the values of its image. *)
  val get_typed_ienv :
    in_channel ->
    in_channel ->
    interactive_env * Store.store * name_ctx * name_ctx
end

module type NF = sig
  type ('value, 'ectx, 'fname, 'cname) nf_term

  val pp_nf_term :
    pp_dir:(Format.formatter -> unit) ->
    (Format.formatter -> 'value -> unit) ->
    (Format.formatter -> 'ectx -> unit) ->
    (Format.formatter -> 'fname -> unit) ->
    (Format.formatter -> 'cname -> unit) ->
    Format.formatter ->
    ('value, 'ectx, 'fname, 'cname) nf_term ->
    unit

  (* The first argument is a string inserted between
     the negative part of the normal form
     and the values filling the positive parts *)
  val string_of_nf_term :
    string ->
    ('value -> string) ->
    ('ectx -> string) ->
    ('fname -> string) ->
    ('cname -> string) ->
    ('value, 'ectx, 'fname, 'cname) nf_term ->
    string

  val is_error : ('value, 'ectx, 'fname, 'cname) nf_term -> bool

  val map :
    f_cn:('cname -> 'cnameb) ->
    f_fn:('fname -> 'fnameb) ->
    f_val:('value -> 'valueb) ->
    f_ectx:('ectx -> 'ectxb) ->
    ('value, 'ectx, 'fname, 'cname) nf_term ->
    ('valueb, 'ectxb, 'fnameb, 'cnameb) nf_term

  val merge_val_ectx :
    f_ret:('value -> 'valueb) ->
    f_call:('value * 'ectx -> 'valueb) ->
    ('value, 'ectx, 'fname, 'cname) nf_term ->
    ('valueb, unit, 'fname, 'cname) nf_term

  val apply_val :
    'a -> ('value -> 'a) -> ('value, 'ectx, 'fname, 'cname) nf_term -> 'a

  val map_val :
    'res ->
    ('value -> 'valueb * 'res) ->
    ('value, 'ectx, 'fname, 'cname) nf_term ->
    ('valueb, 'ectx, 'fname, 'cname) nf_term * 'res

  val map_ectx :
    'res ->
    ('ectx -> 'ectxb * 'res) ->
    ('value, 'ectx, 'fname, 'cname) nf_term ->
    ('value, 'ectxb, 'fname, 'cname) nf_term * 'res

  val map_cn :
    'res ->
    ('cname -> 'cnameb * 'res) ->
    ('value, 'ectx, 'fname, 'cname) nf_term ->
    ('value, 'ectx, 'fname, 'cnameb) nf_term * 'res

  val map_fn :
    'res ->
    ('fname -> 'fnameb * 'res) ->
    ('value, 'ectx, 'fname, 'cname) nf_term ->
    ('value, 'ectx, 'fnameb, 'cname) nf_term * 'res

  module M : Util.Monad.BRANCH

  val abstract_nf_term_m :
    gen_val:('value -> ('avalue * 'res) M.m) ->
    ('value, 'ectx, 'fname, 'cname) nf_term ->
    (('avalue, unit, 'fname, 'cname) nf_term * 'res) M.m

  val equiv_nf_term :
    ('name Util.Namespan.namespan ->
    'value ->
    'value ->
    'name Util.Namespan.namespan option) ->
    'name Util.Namespan.namespan ->
    ('value, 'ectx, 'name, 'name) nf_term ->
    ('value, 'ectx, 'name, 'name) nf_term ->
    'name Util.Namespan.namespan option
end

(* The following signature WITHAVAL_INOUT is used for operational languages
   with normal forms that do not have necessarily a head name.
   This is for example the case of terms that are not named, so
   that values are normal forms. *)
module type WITHAVAL_INOUT = sig
  include COMP
  module Nf : NF with module M = Store.M

  type eval_context
  type typevar
  type typename

  val pp_eval_context : Format.formatter -> eval_context -> unit
  val string_of_eval_context : eval_context -> string
  val pp_tvar_l : Format.formatter -> typevar list -> unit
  val string_of_typename : typename -> string

  val type_annotating_val :
    inj_ty:(typ -> 'ty) ->
    fname_ctx:('fname, 'ty) Util.Pmap.pmap ->
    cname_ctx:('cname, 'ty) Util.Pmap.pmap ->
    ('value, 'ectx, 'fname, 'cname) Nf.nf_term ->
    ('value * 'ty, 'ectx, 'fname, 'cname) Nf.nf_term

  val type_annotating_ectx :
    ('fname, typ) Util.Pmap.pmap ->
    typ ->
    ('value, 'ectx, 'fname, 'cname) Nf.nf_term ->
    ('value, 'ectx * (typ * typ), 'fname, 'cname) Nf.nf_term

  val type_check_nf_term :
    inj_ty:(typ -> 'ty) ->
    empty_res:'res ->
    fname_ctx:('fname, 'nty) Util.Pmap.pmap ->
    cname_ctx:('cname, 'ty * 'ty) Util.Pmap.pmap ->
    type_check_call:('value -> 'nty -> 'res option) ->
    type_check_ret:('value -> 'ty -> 'ty -> 'res option) ->
    ('value, 'ectx, 'fname, 'cname) Nf.nf_term ->
    'res option

  val generate_nf_term_call :
    ('fname, 'typ * 'typ) Util.Pmap.pmap ->
    (('typ, unit, 'fname, 'cname) Nf.nf_term * 'typ) Nf.M.m

  val generate_nf_term_ret :
    (typ -> 'ty) ->
    ('cname, 'ty * 'ty) Util.Pmap.pmap ->
    (('ty, 'ectx, 'fname, 'cname) Nf.nf_term * 'ty) Nf.M.m

  val generate_typename_subst :
    typevar list -> typename list * (typevar, typ) Util.Pmap.pmap

  val apply_type_subst : typ -> (typevar, typ) Util.Pmap.pmap -> typ
  val get_input_type : negative_type -> typevar list * typ
  val get_output_type : negative_type -> typ

  type normal_form_term = (value, eval_context, Name.name, unit) Nf.nf_term

  val get_nf_term : term -> normal_form_term

  val refold_nf_term :
    (value, unit, negative_val, eval_context) Nf.nf_term -> term

  module AVal :
    Abstract_val.AVAL
      with type name = Name.name
       and type value = value
       and type negative_val = negative_val
       and type typ = typ
       and type negative_type = negative_type
       and type label = Store.label
       and type store_ctx = Store.store_ctx
       and module M = Store.M
end

(* The following signature WITHAVAL_NEG is used for operational languages
   with normal forms that have necessarily a head name.
   This is for example the case of languages with named terms,
   as in the λμ-calculus. *)

module type WITHAVAL_NEG = sig
  include COMP
  module Nf : NF with module M = Store.M

  val type_annotating_val :
    (Name.name, typ) Util.Pmap.pmap ->
    ('value, 'ectx, Name.name, Name.name) Nf.nf_term ->
    ('value * typ, 'ectx, Name.name, Name.name) Nf.nf_term

  val type_check_nf_term :
    empty_res:'res ->
    name_ctx:(Name.name, negative_type) Util.Pmap.pmap ->
    type_check_val:('value -> negative_type -> 'res option) ->
    ('value, 'ectx, Name.name, Name.name) Nf.nf_term ->
    'res option

  val generate_nf_term :
    (Name.name, typ) Util.Pmap.pmap ->
    (typ, unit, Name.name, Name.name) Nf.nf_term Nf.M.m

  val negating_type : negative_type -> typ

  (*conf_type is ⊥*)
  val conf_type : typ

  type normal_form_term = (value, unit, Name.name, Name.name) Nf.nf_term

  val get_nf_term : term -> normal_form_term

  val refold_nf_term :
    (value, unit, negative_val, negative_val) Nf.nf_term -> term

  module AVal :
    Abstract_val.AVAL
      with type name = Name.name
       and type value = value
       and type negative_val = negative_val
       and type typ = typ
       and type negative_type = negative_type
       and type label = Store.label
       and type store_ctx = Store.store_ctx
       and module M = Store.M
end
