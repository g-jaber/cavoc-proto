module Make (OpLang : Language.WITHAVAL_INOUT) : Interactive.LANG = struct
  open OpLang
  module Name = OpLang.Name
  module M = AVal.M
  module Store = OpLang.Store

  type computation = term

  let string_of_computation = string_of_term

  (* A stack context (σ1,τ1)::(σ2,τ2)::...::(σn,τn)
     is the typing context for the stack of evaluation contexts,
     with σi the type of the hole and τi the return type )*)
  type stack_ctx = (OpLang.typ * OpLang.typ) list

  type name_ctx =
    | OpCtx of OpLang.typ option * OpLang.name_ctx
    | PropCtx of OpLang.name_ctx * stack_ctx

  let empty_name_ctx = PropCtx (OpLang.empty_name_ctx, [])

  let string_of_stack_ctx = function
    | [] -> ""
    | stackctx ->
        let string_l =
          List.map
            (fun (ty1, ty2) ->
              OpLang.string_of_type ty1 ^ "->" ^ OpLang.string_of_type ty2)
            stackctx in
        String.concat "::" string_l

  let string_of_name_ctx = function
    | OpCtx (ty_option, fnamectx) ->
        let ty_string =
          match ty_option with
          | None -> ""
          | Some ty -> OpLang.string_of_type ty ^ "|" in
        ty_string ^ OpLang.string_of_name_ctx fnamectx
    | PropCtx (fnamectx, stackctx) ->
        string_of_stack_ctx stackctx ^ "|" ^ OpLang.string_of_name_ctx fnamectx

  let concat_name_ctx namectx1 namectx2 =
    match (namectx1, namectx2) with
    | (PropCtx (fnamectx1, stackctx1), PropCtx (fnamectx2, stackctx2)) ->
        PropCtx
          (OpLang.concat_name_ctx fnamectx1 fnamectx2, stackctx1 @ stackctx2)
    | (OpCtx (ty, fnamectx1), OpCtx (_, fnamectx2)) ->
        OpCtx (ty, OpLang.concat_name_ctx fnamectx1 fnamectx2)
    | _ ->
        failwith
        @@ "Trying to concatenate two interactive typing contexts of different \
            polarities: "
        ^ string_of_name_ctx namectx1
        ^ " and "
        ^ string_of_name_ctx namectx2
        ^ ". Please report."

  let get_names_from_name_ctx = function
    | PropCtx (fnamectx, _) | OpCtx (_, fnamectx) ->
        OpLang.get_names_from_name_ctx fnamectx

  (* Interactive environments γ are pairs formed by partial maps from functional names to functional values,
     and a stack of evaluation contexts. *)
  type interactive_env = OpLang.interactive_env * OpLang.eval_context list

  let empty_ienv = (OpLang.empty_ienv, [])

  let concat_ienv (fnamectx1, cstack1) (fnamectx2, cstack2) =
    (OpLang.concat_ienv fnamectx1 fnamectx2, cstack1 @ cstack2)

    let string_of_stack = function
    | [] -> ""
    | stackctx ->
        let string_l =
          List.map
            (fun ectx ->
              OpLang.string_of_eval_context ectx)
            stackctx in
        String.concat "::" string_l

  let string_of_interactive_env (namectx, stackctx) =
    string_of_stack stackctx ^ " | "  ^OpLang.string_of_interactive_env namectx

  type normal_form =
    (value, eval_context, Name.name, unit) OpLang.Nf.nf_term * Store.store

  let is_error (nf_term, _) = OpLang.Nf.is_error nf_term
  let get_store (_, store) = store

  let compute_nf (term, store) =
    match normalize_opconf (term, store) with
    | None -> None
    | Some (nf_term, store') -> Some (OpLang.get_nf_term nf_term, store')

  let string_of_nf (nf_term, _) =
    let f_call (fn, value, ectx) =
      OpLang.Name.string_of_name fn
      ^ "("
      ^ OpLang.string_of_value value
      ^ ","
      ^ OpLang.string_of_eval_context ectx
      ^ ")" in
    let f_ret ((), value) = "ret(" ^ OpLang.string_of_value value ^ ")" in
    let f_exn ((), value) = "raise(" ^ OpLang.string_of_value value ^ ")" in
    let f_error () = "error" in
    OpLang.Nf.apply_cons ~f_call ~f_ret ~f_exn ~f_error nf_term

  let concretize_a_nf ((fname_env, stack_ctx) as ienv) (a_nf_term, store) =
    let get_ectx () = match stack_ctx with
    | ectx :: stack_ctx' -> (ectx,(fname_env,stack_ctx'))
    | [] ->
        failwith
          "Error: trying to concretize a returning abstract normal form in \
           an empty stack. Please report"
  in
    let f_call (fn,aval,()) = 
      let funval =  Util.Pmap.lookup_exn fn fname_env in
      let value = AVal.subst_names fname_env aval in
      ((funval,value,()),ienv)
    in
    let f_ret ((),aval) = 
      let value = AVal.subst_names fname_env aval in 
      let (ectx,ienv') = get_ectx () in ((ectx,value),ienv') in
    let f_exn = f_ret in
    let f_error = get_ectx in
    let (nf_term',ienv') = OpLang.Nf.map_cons ~f_call ~f_ret ~f_exn ~f_error a_nf_term in
    (refold_nf_term nf_term', store, ienv')

  type abstract_normal_form =
    (AVal.abstract_val, unit, Name.name, unit) OpLang.Nf.nf_term * Store.store

  let labels_of_a_nf_term = OpLang.Nf.apply_val [] AVal.labels_of_abstract_val
  let get_store_of_a_nf (_, store) = store
  let abstracting_store = OpLang.Store.restrict
  (* TODO: Deal with the abstraction process of the heap properly *)

  let[@warning "-8"] abstracting_nf_term nf_term
      (OpCtx (Some ty_out, fnamectxO)) =
    let f_call (fn, value, ectx) =
      let ty = Util.Pmap.lookup_exn fn fnamectxO in
      let (_, ty_in) = get_input_type ty in
      let (aval, ienv, lnamectx) = AVal.abstracting_value value ty_in in
      let ty_hole = get_output_type ty in
      (* TODO: we should something with the tvar_l *)
      let res = ((ienv, [ ectx ]), PropCtx (lnamectx, [ (ty_hole, ty_out) ])) in
      ((fn, aval, ()), res) in
    let f_ret ((), value) =
      let (aval, ienv, lnamectx) = AVal.abstracting_value value ty_out in
      let res = ((ienv, []), PropCtx (lnamectx, [])) in
      (((), aval), res) in
    let f_exn ((), value) =
      let (aval, ienv, lnamectx) =
        AVal.abstracting_value value OpLang.exception_type in
      (((), aval), ((ienv, []), PropCtx (lnamectx, []))) in
    let f_error () = ((), (empty_ienv, empty_name_ctx)) in
    OpLang.Nf.map_cons ~f_call ~f_ret ~f_exn ~f_error nf_term

  let abstracting_normal_form (nf_term, store) namectxO storectx_discl =
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
    let f_call (fn, _, _) = Some fn in
    let f_ret _ = None in
    let f_exn _ = None in
    let f_error _ = None in
    OpLang.Nf.apply_cons ~f_call ~f_ret ~f_exn ~f_error a_nf_term

  let get_support (a_nf_term, _) =
    OpLang.Nf.apply_val [] AVal.names_of_abstract_val a_nf_term
  (*TODO: take into account the store part*)

  let string_of_a_nf dir (nf_term, store) =
    let f_call (fn, aval, ()) =
      OpLang.Name.string_of_name fn
      ^ dir ^ "("
      ^ OpLang.AVal.string_of_abstract_val aval
      ^ ")" in
    let f_ret ((), aval) =
      "ret" ^ dir ^ "(" ^ OpLang.AVal.string_of_abstract_val aval ^ ")" in
    let f_exn ((), aval) =
      "raise" ^ dir ^ "(" ^ OpLang.AVal.string_of_abstract_val aval ^ ")" in
    let f_error _ = "error" in
    let string_nf_term =
      OpLang.Nf.apply_cons ~f_call ~f_ret ~f_exn ~f_error nf_term in
    if store = Store.empty_store then string_nf_term
    else
      let string_store = Store.string_of_store store in
      string_nf_term ^ "," ^ string_store

  include AVal.M

  let fill_abstract_val storectx fnamectxP nf_skeleton =
    let f_val ((_, in_ty), out_ty, namectxP') =
      (*TODO: We should take into account the type var list*)
      let* (aval, lnamectx) =
        AVal.generate_abstract_val storectx fnamectxP in_ty in
      return (aval, (OpCtx (Some out_ty, lnamectx), namectxP')) in
    let f_fn (_, ty, ()) = f_val ty in
    let f_cn ((), ty) = f_val ty in
    OpLang.Nf.abstract_nf_term_m ~f_fn ~f_cn nf_skeleton

  let[@warning "-8"] generate_a_nf storectx
      (PropCtx (fnamectx, stackctx) as namectxP) =
    let fname_ctx =
      Util.Pmap.filter_map
        (fun (nn, ty) ->
          if OpLang.Name.is_fname nn then
            Some
              ( nn,
                ( (OpLang.get_input_type ty, OpLang.get_output_type ty, namectxP),
                  () ) )
          else None)
        fnamectx in
    let (exn_ctx, cname_ctx) =
      match stackctx with
      | [] -> (Util.Pmap.empty, Util.Pmap.empty)
      | (ty1, ty2) :: stackctx' ->
          let namectxP' = PropCtx (fnamectx, stackctx') in
          ( Util.Pmap.singleton
              ((), (([], OpLang.exception_type), ty2, namectxP')),
            Util.Pmap.singleton ((), (([], ty1), ty2, namectxP')) ) in
    let* skel = OpLang.Nf.generate_nf_term ~fname_ctx ~exn_ctx ~cname_ctx in
    let* (a_nf_term, (lnamectx, namectxP')) =
      fill_abstract_val storectx fnamectx skel in
    let* store = Store.generate_store storectx in
    return ((a_nf_term, store), lnamectx, namectxP')

  let[@warning "-8"] type_check_a_nf
      (PropCtx (fnamectxP, stack_ctx) as namectxP) (OpCtx (_, fnamectxO))
      (a_nf, _) =
    let lift_lnamectx namectxP' ty_out = function
      | None -> None
      | Some lnamectx -> Some (OpCtx (Some ty_out, lnamectx), namectxP') in
    let f_call (fn, aval, ()) =
      let nty = Util.Pmap.lookup_exn fn fnamectxO in
      let (_, ty) = get_input_type nty in
      let ty_out = get_output_type nty in
      lift_lnamectx namectxP ty_out
      @@ AVal.type_check_abstract_val fnamectxP fnamectxO ty aval in
    let f_ret ((), aval) =
      match stack_ctx with
      | [] -> None
      | (ty_hole, ty_out) :: stack_ctx' ->
          let namectxP' = PropCtx (fnamectxP, stack_ctx') in
          lift_lnamectx namectxP' ty_out
          @@ AVal.type_check_abstract_val fnamectxP fnamectxO ty_hole aval in
    let f_exn ((), aval) =
      match stack_ctx with
      | [] -> None
      | (_, ty_out) :: stack_ctx' ->
          let namectxP' = PropCtx (fnamectxP, stack_ctx') in
          lift_lnamectx namectxP' ty_out
          @@ AVal.type_check_abstract_val fnamectxP fnamectxO
               OpLang.exception_type aval in
    let f_error () = Some (empty_name_ctx, namectxP) in
    OpLang.Nf.apply_cons ~f_call ~f_ret ~f_exn ~f_error a_nf

  (* Beware that is_equiv_a_nf does not check the equivalence of
     the store part of abstract normal forms.
     This is needed for the POGS equivalence. *)
  let is_equiv_a_nf _ (_, _) (_, _) = failwith "Not yet implemented"

  let get_typed_interactive_env inBuffer_implem inBuffer_signature =
    let (ienv, store, namectxP, namectxO) =
      OpLang.get_typed_interactive_env inBuffer_implem inBuffer_signature in
    ((ienv, []), store, PropCtx (namectxP, []), OpCtx (None, namectxO))

  let get_typed_term nbprog inBuffer =
    let (comp, ty, namectxO) = OpLang.get_typed_term nbprog inBuffer in
    (comp, OpCtx (Some ty, namectxO))
end
