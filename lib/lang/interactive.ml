module type LANG = sig
  module Name : Names.CONT_NAMES

  type computation

  val string_of_computation : computation -> string

  module M : Util.Monad.BRANCH
  module Store : Language.STORE with module M = M

  type normal_form

  val string_of_nf : normal_form -> string

  (* Error normal form are the one that cannot interact with the environment*)
  val is_error : normal_form -> bool
  val get_store : normal_form -> Store.store

  (* compute_nf computes the normal form of an operational configuration,
     or None when we detect that the operational configuration diverges.*)
  val compute_nf : computation * Store.store -> normal_form option

  (*Interactive name contexts are typing contexts mapping names to interactive types.*)
  type name_ctx

  val empty_name_ctx : name_ctx
  val concat_name_ctx : name_ctx -> name_ctx -> name_ctx
  val string_of_name_ctx : name_ctx -> string
  val get_names_from_name_ctx : name_ctx -> Name.name list

  (* Interactive environments γ are partial maps from names to interactive values*)
  type interactive_env

  val empty_ienv : interactive_env
  val concat_ienv : interactive_env -> interactive_env -> interactive_env
  val string_of_ienv : interactive_env -> string

  (* The typed focusing process implemented by abstracting_nf
      decomposes a normal form into:
       - an abstract normal form for the observable part;
       - a typed interactive environment for the negative part. *)

  (* abstract normal forms can be thought of named copatterns when the language is
     of signature Language.WITHAVAL_INOUT. *)

  type abstract_normal_form

  val abstracting_nf :
    normal_form ->
    name_ctx ->
    Store.store_ctx ->
    (abstract_normal_form * interactive_env * name_ctx * Store.store_ctx) option

  val get_subject_name : abstract_normal_form -> Name.name option
  val get_support : abstract_normal_form -> Name.name list

  (* The first argument is a string inserted between
     the negative part of the normal form
     and the abstract values filling the positive parts *)
  val string_of_a_nf : string -> abstract_normal_form -> string

  val is_equiv_a_nf :
    Name.name Util.Namespan.namespan ->
    abstract_normal_form ->
    abstract_normal_form ->
    Name.name Util.Namespan.namespan option

  (* From the interactive name context Γ_P,
     we generate all the possible pairs (A,Δ,Γ'_P) formed by an abstracted normal form A such that
     Γ_P;_ ⊢ A ▷ Δ
     Freshness of names that appear in Δ is guaranteed by a gensym, so that we do not need to provide Γ_O. *)
  val generate_a_nf :
    Store.store_ctx ->
    name_ctx ->
    (abstract_normal_form * name_ctx * name_ctx) M.m

  (* The typing judgment of an abstracted normal form Γ_P;Γ_O ⊢ A ▷ Δ
     produces the interactive name context (Δ,Γ'_P) of fresh names introduced by A.
     It returns None when the type checking fails.
     The context Γ_P is used to retrieve the existing polymorphic names, and to check for freshness of other names.
     The contexts Γ_O is used to check for freshness of names *)

  val type_check_a_nf :
    name_ctx -> name_ctx -> abstract_normal_form -> (name_ctx * name_ctx) option

  val concretize_a_nf :
    interactive_env ->
    abstract_normal_form ->
    computation * Store.store * interactive_env

  val get_store_of_a_nf : abstract_normal_form -> Store.store
  val get_typed_term : string -> in_channel -> computation * name_ctx

  (* The function get_typed_ienv
     retrive a module declaration and its signature from the two in_channel taken as input.
     It evaluates the list of computation declarations
     into a list of value declarations together with the store
     generated by this evaluation.
     We return a Proponent and an Opponent name context. *)
  val get_typed_ienv :
    in_channel ->
    in_channel ->
    interactive_env * Store.store * name_ctx * name_ctx
end

(* The following functor create a module of type Interactive.LANG
   from a module OpLang of type Language.WITHAVAL_NEG *)
module Make (OpLang : Language.WITHAVAL_NEG) : LANG = struct
  (*open OpLang*)
  module Name = OpLang.Name
  module M = OpLang.AVal.M
  module Store = OpLang.Store

  type computation = OpLang.term

  let string_of_computation = OpLang.string_of_term

  type name_ctx = OpLang.name_ctx

  let empty_name_ctx = OpLang.empty_name_ctx
  let concat_name_ctx = OpLang.concat_name_ctx
  let string_of_name_ctx = OpLang.string_of_name_ctx
  let get_names_from_name_ctx = OpLang.get_names_from_name_ctx

  (* Interactive environments γ are partial maps from names to interactive values*)
  type interactive_env = OpLang.interactive_env

  let empty_ienv = OpLang.empty_ienv
  let concat_ienv = OpLang.concat_ienv
  let string_of_ienv = OpLang.string_of_ienv

  type normal_form = OpLang.normal_form_term * OpLang.Store.store

  let is_error (nf_term, _) = OpLang.Nf.is_error nf_term
  let get_store (_, store) = store

  let compute_nf (term, store) =
    match OpLang.normalize_opconf (term, store) with
    | None -> None
    | Some (nf_term, store') -> Some (OpLang.get_nf_term nf_term, store')

  let string_of_nf_term dir string_of_val nf_term =
    let f_call (fn, value, ()) =
      OpLang.Name.string_of_name fn ^ dir ^ string_of_val value in
    let f_ret (cn, value) = Name.string_of_name cn ^ dir ^ string_of_val value in
    let f_exn (cn, value) =
      Name.string_of_name cn ^ dir ^ "raise" ^ string_of_val value in
    let f_error cn = Name.string_of_name cn ^ dir ^ "error" in
    OpLang.Nf.apply_cons ~f_call ~f_ret ~f_exn ~f_error nf_term

  let string_of_nf (nf_term, _) = string_of_nf_term "" OpLang.string_of_value nf_term

  let concretize_a_nf ienv (a_nf_term, store) =
    let f_val = OpLang.AVal.subst_names ienv in
    let f_fn nn = Util.Pmap.lookup_exn nn ienv in
    let f_cn = f_fn in
    let f_ectx () = () in
    let nf_term' = OpLang.Nf.map ~f_val ~f_fn ~f_cn ~f_ectx a_nf_term in
    (OpLang.refold_nf_term nf_term', store, ienv)

  type abstract_normal_form =
    (OpLang.AVal.abstract_val, unit, Name.name, Name.name) OpLang.Nf.nf_term
    * Store.store

  let labels_of_a_nf_term = OpLang.Nf.apply_val [] OpLang.AVal.labels_of_abstract_val
  let get_store_of_a_nf (_, store) = store
  let abstracting_store = OpLang.Store.restrict
  (* TODO Deal with the abstraction process of the heap properly *)

  let abstracting_nf_term nf_term namectxO =
    let f_call (nn, value, ()) =
      let ty = Util.Pmap.lookup_exn nn namectxO in
      let nty = OpLang.negating_type ty in
      let (aval, ienv, lnamectx) = OpLang.AVal.abstracting_value value nty in
      ((nn, aval, ()), (ienv, lnamectx)) in
    let f_ret (nn, value) =
      let ty = Util.Pmap.lookup_exn nn namectxO in
      let nty = OpLang.negating_type ty in
      let (aval, ienv, lnamectx) = OpLang.AVal.abstracting_value value nty in
      ((nn, aval), (ienv, lnamectx)) in
    let f_exn (nn, value) =
      let (aval, ienv, lnamectx) =
        OpLang.AVal.abstracting_value value OpLang.exception_type in
      ((nn, aval), (ienv, lnamectx)) in
    let f_error nn = (nn, (empty_ienv, empty_name_ctx)) in
    OpLang.Nf.map_cons ~f_call ~f_ret ~f_exn ~f_error nf_term

  let abstracting_nf (nf_term, store) namectxO storectx_discl =
    let (a_nf_term, (ienv, lnamectx)) = abstracting_nf_term nf_term namectxO in
    if OpLang.Nf.is_error a_nf_term then None
    else
      let label_l = labels_of_a_nf_term a_nf_term in
      let storectx = OpLang.Store.infer_type_store store in
      Util.Debug.print_debug @@ "The full store context is "
      ^ OpLang.Store.string_of_store_ctx storectx;
      let storectx_discl' = OpLang.Store.restrict_ctx storectx label_l in
      let storectx_discl'' =
        OpLang.Store.concat_store_ctx storectx_discl storectx_discl' in
      Util.Debug.print_debug @@ "The new diclosed store context is "
      ^ OpLang.Store.string_of_store_ctx storectx_discl'';
      let store_discl = abstracting_store storectx_discl' store in
      Some ((a_nf_term, store_discl), ienv, lnamectx, storectx_discl'')

  (* Notice that the disclosure process is in fact more complex
     since the image of  store_discl might itself has
     labels that becomes diclosed.
     This computation would necessitate an iterative process. *)

  let get_subject_name (a_nf_term, _) =
    let f_call (nn, _, _) = nn in
    let f_ret (nn, _) = nn in
    let f_exn (nn, _) = nn in
    let f_error nn = nn in
    Some (OpLang.Nf.apply_cons ~f_call ~f_ret ~f_exn ~f_error a_nf_term)

  let get_support (a_nf_term, _) =
    OpLang.Nf.apply_val [] OpLang.AVal.names_of_abstract_val a_nf_term
  (*TODO: take into account the store part*)

  let string_of_a_nf dir (a_nf_term, store) =
    let string_nf_term =
      string_of_nf_term dir OpLang.AVal.string_of_abstract_val a_nf_term in
    if store = Store.empty_store then string_nf_term
    else
      let string_store = Store.string_of_store store in
      string_nf_term ^ "," ^ string_store

  include OpLang.AVal.M

  let generate_nf_skeleton namectxP =
    let fname_ctx =
      Util.Pmap.filter_map
        (fun (fn, ty) ->
          if Name.is_fname fn then Some (fn, (OpLang.negating_type ty, ()))
          else None)
        namectxP in
    let cname_ctx =
      Util.Pmap.filter_map
        (fun (cn, ty) ->
          if Name.is_cname cn then Some (cn, OpLang.negating_type ty) else None)
        namectxP in
    let exn_ctx = Util.Pmap.map_im (fun _ -> OpLang.exception_type) cname_ctx in
    OpLang.Nf.generate_nf_term ~fname_ctx ~cname_ctx ~exn_ctx

  let fill_abstract_val storectx namectxP nf_skeleton =
    let f_fn (_, ty, _) = OpLang.AVal.generate_abstract_val storectx namectxP ty in
    let f_cn (_, ty) = OpLang.AVal.generate_abstract_val storectx namectxP ty in
    OpLang.Nf.abstract_nf_term_m ~f_fn ~f_cn nf_skeleton

  let generate_a_nf storectx namectxP =
    let* skel = generate_nf_skeleton namectxP in
    let* (a_nf_term, lnamectx) = fill_abstract_val storectx namectxP skel in
    let* store = Store.generate_store storectx in
    return ((a_nf_term, store), lnamectx, namectxP)

  let type_check_a_nf namectxP namectxO (a_nf, _) =
    let aux nn aval =
      let nty = Util.Pmap.lookup_exn nn namectxO in
      let ty = OpLang.negating_type nty in
      OpLang.AVal.type_check_abstract_val namectxP namectxO ty aval in
    let f_call (fn, aval, ()) = aux fn aval in
    let f_ret (cn, aval) = aux cn aval in
    let f_exn (_, aval) =
      OpLang.AVal.type_check_abstract_val namectxP namectxO OpLang.exception_type aval
    in
    let f_error _ = Some Util.Pmap.empty in
    match OpLang.Nf.apply_cons ~f_call ~f_ret ~f_exn ~f_error a_nf with
    | None -> None
    | Some lnamectx -> Some (lnamectx, namectxP)
  (*TODO: Type check the store part and
     check that the disclosure process is respected*)

  (* Beware that is_equiv_a_nf does not check the equivalence of
     the store part of abstract normal forms.
     This is needed for the POGS equivalence. *)
  let is_equiv_a_nf span (anf1, _) (anf2, _) =
    OpLang.Nf.equiv_nf_term OpLang.AVal.unify_abstract_val span anf1 anf2

  let get_typed_ienv = OpLang.get_typed_ienv

  let get_typed_term nbprog inBuffer =
    let (comp, _, namectxO) = OpLang.get_typed_term nbprog inBuffer in
    (comp, namectxO)
end
