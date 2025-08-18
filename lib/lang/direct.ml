module Make (OpLang : Language.WITHAVAL_INOUT) : Interactive.LANG = struct
  open OpLang
  module EvalMonad = OpLang.EvalMonad
  module Names = OpLang.Names
  module BranchMonad = AVal.BranchMonad

  type computation = term
  type opconf = computation * Store.store

  let pp_opconf fmt (term, store) =
    Format.fprintf fmt "@[(@[Computation: %a@] @| @[Store: %a@])@]"
      OpLang.pp_term term Store.pp_store store

  let string_of_opconf = Format.asprintf "%a" pp_opconf

  type store = OpLang.Store.store

  let string_of_store = OpLang.Store.string_of_store
  let pp_store = OpLang.Store.pp_store

  type store_ctx = OpLang.Store.store_ctx

  let string_of_store_ctx = OpLang.Store.string_of_store_ctx
  let pp_store_ctx = OpLang.Store.pp_store_ctx
  let empty_store_ctx = OpLang.Store.empty_store_ctx
  let infer_type_store = OpLang.Store.infer_type_store

  (* A stack context (σ1,τ1)::(σ2,τ2)::...::(σn,τn)
     is the typing context for the stack of evaluation contexts,
     with σi the type of the hole and τi the return type )*)
  type stack_ctx = (OpLang.typ * OpLang.typ) list

  let stack_ctx_to_yojson stack =
    let to_string (ty1, ty2) =
      `String (OpLang.string_of_type ty1 ^ " ⇝ " ^ OpLang.string_of_type ty2)
    in
    `List (List.map to_string stack)

  (* The active context PropCtx have a type for the toplevel term*)
  type name_ctx =
    | OpCtx of OpLang.typ option * OpLang.name_ctx
    | PropCtx of OpLang.name_ctx * stack_ctx

  let name_ctx_to_yojson = function
    | OpCtx (None, name_ctx) -> OpLang.name_ctx_to_yojson name_ctx
    | OpCtx (Some ty, name_ctx) ->
        `Assoc
          [
            ("type", `String (OpLang.string_of_type ty));
            ("name_ctx", OpLang.name_ctx_to_yojson name_ctx);
          ]
    | PropCtx (name_ctx, stack_ctx) ->
        `Assoc
          [
            ("name_ctx", OpLang.name_ctx_to_yojson name_ctx);
            ("stack_ctx", stack_ctx_to_yojson stack_ctx);
          ]

  let empty_name_ctx = PropCtx (OpLang.empty_name_ctx, [])

  let pp_stack_ctx fmt = function
    | [] -> Format.fprintf fmt "⋅"
    | stack_ctx ->
        let pp_pair fmt (ty1, ty2) =
          Format.fprintf fmt "%a ⇝ %a" OpLang.pp_type ty1 OpLang.pp_type ty2
        in
        Format.pp_print_list pp_pair fmt stack_ctx

  let pp_name_ctx fmt = function
    | OpCtx (None, name_ctx) -> OpLang.pp_name_ctx fmt name_ctx
    | OpCtx (Some ty, name_ctx) ->
        Format.fprintf fmt "%a | %a" OpLang.pp_type ty OpLang.pp_name_ctx
          name_ctx
    | PropCtx (name_ctx, stack_ctx) ->
        Format.fprintf fmt "%a | %a" OpLang.pp_name_ctx name_ctx pp_stack_ctx
          stack_ctx

  let string_of_name_ctx = Format.asprintf "%a" pp_name_ctx

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

  let interactive_env_to_yojson (ienv, ectx_l) =
    let ectx_l_yojson =
      `List
        (List.map (fun x -> `String (OpLang.string_of_eval_context x)) ectx_l)
    in
    `Assoc
      [
        ("ienv", OpLang.interactive_env_to_yojson ienv);
        ("ectx stack", ectx_l_yojson);
      ]

  let empty_ienv = (OpLang.empty_ienv, [])

  let concat_ienv (fnamectx1, cstack1) (fnamectx2, cstack2) =
    (OpLang.concat_ienv fnamectx1 fnamectx2, cstack1 @ cstack2)

  let pp_ctx_stack fmt = function
    | [] -> Format.pp_print_string fmt "⋅"
    | ectx_stack ->
        let pp_sep fmt () = Format.pp_print_char fmt ',' in
        Format.pp_print_list ~pp_sep OpLang.pp_eval_context fmt ectx_stack

  let pp_ienv fmt (ienv, ectx_stack) =
    Format.fprintf fmt "@[IEnv: %a@] | @[Stack: %a@]" OpLang.pp_ienv ienv
      pp_ctx_stack ectx_stack

  let string_of_ienv = Format.asprintf "%a" pp_ienv

  let concretize_a_nf store ((fname_env, stack_ctx) as ienv) (a_nf_term, store')
      =
    let get_ectx () =
      match stack_ctx with
      | ectx :: stack_ctx' -> (ectx, (fname_env, stack_ctx'))
      | [] ->
          failwith
            "Error: trying to concretize a returning abstract normal form in \
             an empty stack. Please report" in
    let f_val aval = (AVal.subst_names fname_env aval, ()) in
    let f_fn fn = (Util.Pmap.lookup_exn fn fname_env, ()) in
    let f_cn () = get_ectx () in
    let (nf_term, _) = OpLang.Nf.map_val () f_val a_nf_term in
    let (nf_term', _) = OpLang.Nf.map_fn () f_fn nf_term in
    let (nf_term'', ienv') = OpLang.Nf.map_cn ienv f_cn nf_term' in
    Util.Debug.print_debug "Updating the store";
    let newstore = Store.update_store store store' in
    ((refold_nf_term nf_term'', newstore), ienv')

  type abstract_normal_form =
    (AVal.abstract_val, unit, Names.name, unit) OpLang.Nf.nf_term * Store.store

  let labels_of_a_nf_term = OpLang.Nf.apply_val [] AVal.labels_of_abstract_val
  let abstracting_store = OpLang.Store.restrict
  (* TODO: Deal with the abstraction process of the heap properly *)

  let[@warning "-8"] abstracting_nf_term nf_term
      (OpCtx (Some ty_out, fnamectxO)) =
    let inj_ty ty = ty in
    let fname_ctx =
      Util.Pmap.map_im (fun nty -> snd @@ get_input_type nty) fnamectxO in
    (* TODO: we should do something with the tvar_l *)
    let fname_ctx_hole =
      Util.Pmap.map_im (fun nty -> get_output_type nty) fnamectxO in
    let cname_ctx = Util.Pmap.singleton ((), ty_out) in
    let nf_typed_term =
      OpLang.type_annotating_val ~inj_ty ~fname_ctx ~cname_ctx nf_term in
    let nf_typed_term' =
      OpLang.type_annotating_ectx fname_ctx_hole ty_out nf_typed_term in
    let f_val (value, nty) =
      let (aval, ienv, lnamectx) = OpLang.AVal.abstracting_value value nty in
      (aval, (ienv, lnamectx)) in
    let f_ectx (ectx, (ty_hole, ty_out)) =
      ((), ([ ectx ], [ (ty_hole, ty_out) ])) in
    let empty_res = (Util.Pmap.empty, Util.Pmap.empty) in
    let (a_nf_term, (ienv, lnamectx)) =
      OpLang.Nf.map_val empty_res f_val nf_typed_term' in
    let (a_nf_term', (stack, stack_ctx)) =
      OpLang.Nf.map_ectx ([], []) f_ectx a_nf_term in
    (a_nf_term', ((ienv, stack), PropCtx (lnamectx, stack_ctx)))

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
    let f_fn nn = (nn, Some nn) in
    snd @@ OpLang.Nf.map_fn None f_fn a_nf_term

  let get_support (a_nf_term, _) =
    OpLang.Nf.apply_val [] AVal.names_of_abstract_val a_nf_term
  (*TODO: take into account the store part*)

  let pp_a_nf ~pp_dir fmt (a_nf_term, store) =
    let pp_ectx fmt () = Format.pp_print_string fmt "" in
    let pp_cn fmt () = Format.pp_print_string fmt "ret" in
    let pp_a_nf_term =
      OpLang.Nf.pp_nf_term ~pp_dir OpLang.AVal.pp_abstract_val pp_ectx
        OpLang.Names.pp_name pp_cn in
    if store = Store.empty_store then pp_a_nf_term fmt a_nf_term
    else Format.fprintf fmt "%a,%a" pp_a_nf_term a_nf_term Store.pp_store store

  let string_of_a_nf dir =
    let pp_dir fmt = Format.pp_print_string fmt dir in
    Format.asprintf "%a" (pp_a_nf ~pp_dir)

  let eval (opconf, namectxO, storectx_discl) =
    let open EvalMonad in
    let* (term', store') = OpLang.normalize_opconf opconf in
    let nf_term = OpLang.get_nf_term term' in
    match abstracting_nf (nf_term, store') namectxO storectx_discl with
    | Some ((a_nf_term, discl_store), ienv, lnamectx, storectx_discl) ->
        return
          (((a_nf_term, discl_store), lnamectx, storectx_discl), ienv, store')
    | None -> fail ()

  let fill_abstract_val storectx fnamectxP nf_skeleton =
    let gen_val in_ty =
      (*TODO: We should take into account the type var list*)
      AVal.generate_abstract_val storectx fnamectxP in_ty in
    OpLang.Nf.abstract_nf_term_m ~gen_val nf_skeleton

  let[@warning "-8"] generate_a_nf_call storectx
      (PropCtx (fnamectx, _) as namectxP) =
    let fname_ctx =
      Util.Pmap.filter_map
        (fun (nn, ty) ->
          if OpLang.Names.is_fname nn then
            let (_, in_ty) = OpLang.get_input_type ty in
            Some (nn, (in_ty, OpLang.get_output_type ty))
          else None)
        fnamectx in
    let open BranchMonad in
    let* (skel, typ) = OpLang.generate_nf_term_call fname_ctx in
    let* (a_nf_term, lnamectx) = fill_abstract_val storectx fnamectx skel in
    let* store = Store.generate_store storectx in
    let namectxO = OpCtx (Some typ, lnamectx) in
    return ((a_nf_term, store), namectxO, namectxP)

  let[@warning "-8"] generate_a_nf_ret storectx = function
    | PropCtx (_, []) -> BranchMonad.fail ()
    | PropCtx (fnamectx, (ty_hole, ty_out) :: stackctx') ->
        let cname_ctx = Util.Pmap.singleton ((), (ty_hole, ty_out)) in
        let inj_ty ty = ty in
        let open BranchMonad in
        let* (skel, typ) = OpLang.generate_nf_term_ret inj_ty cname_ctx in
        let* (a_nf_term, lnamectx) = fill_abstract_val storectx fnamectx skel in
        let* store = Store.generate_store storectx in
        let namectxP' = PropCtx (fnamectx, stackctx') in
        let namectxO = OpCtx (Some typ, lnamectx) in
        return ((a_nf_term, store), namectxO, namectxP')

  let generate_a_nf storectx namectxP =
    BranchMonad.para_pair
      (generate_a_nf_call storectx namectxP)
      (generate_a_nf_ret storectx namectxP)

  let[@warning "-8"] type_check_a_nf
      (PropCtx (fnamectxP, stack_ctx) as namectxP) (OpCtx (None, fnamectxO))
      (nf_term, _) =
    let inj_ty ty = ty in
    let empty_res = (empty_name_ctx, namectxP) in
    let fname_ctx = fnamectxP in
    let cname_ctx =
      match stack_ctx with
      | [] -> Util.Pmap.empty
      | (ty_hole, ty_out) :: _ -> Util.Pmap.singleton ((), (ty_hole, ty_out))
    in
    let lift_lnamectx namectxP' ty_out = function
      | None -> None
      | Some lnamectx -> Some (OpCtx (Some ty_out, lnamectx), namectxP') in
    let type_check_call aval nty =
      let (_, ty_arg) = OpLang.get_input_type nty in
      let ty_out = OpLang.get_output_type nty in
      lift_lnamectx namectxP ty_out
      @@ AVal.type_check_abstract_val fnamectxP fnamectxO ty_arg aval in
    let type_check_ret aval ty_hole ty_out =
      match stack_ctx with
      | [] -> None
      | _ :: stack_ctx' ->
          let namectxP' = PropCtx (fnamectxP, stack_ctx') in
          lift_lnamectx namectxP' ty_out
          @@ AVal.type_check_abstract_val fnamectxP fnamectxO ty_hole aval in
    OpLang.type_check_nf_term ~inj_ty ~empty_res ~fname_ctx ~cname_ctx
      ~type_check_call ~type_check_ret nf_term

  (* Beware that is_equiv_a_nf does not check the equivalence of
     the store part of abstract normal forms.
     This is needed for the POGS equivalence. *)
  let is_equiv_a_nf _ (_, _) (_, _) = failwith "Not yet implemented"

  let get_typed_ienv lexBuffer_implem lexBuffer_signature =
    let (ienv, store, namectxP, namectxO) =
      OpLang.get_typed_ienv lexBuffer_implem lexBuffer_signature in
    ((ienv, []), store, PropCtx (namectxP, []), OpCtx (None, namectxO))

  let get_typed_opconf nbprog inBuffer =
    let (comp, ty, namectxO) = OpLang.get_typed_term nbprog inBuffer in
    ((comp, Store.empty_store), OpCtx (Some ty, namectxO))
end
