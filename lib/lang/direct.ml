module Make (OpLang : Language.WITHAVAL_INOUT) : Interactive.LANG_WITH_INIT =
struct
  module EvalMonad = OpLang.EvalMonad
  module BranchMonad = OpLang.AVal.BranchMonad

  type computation = OpLang.term
  type store = OpLang.Store.store [@@deriving to_yojson]
  type opconf = computation * OpLang.Store.store

  let pp_opconf fmt (term, store) =
    Format.fprintf fmt "@[(@[Computation: %a@] @| @[Store: %a@])@]"
      OpLang.pp_term term OpLang.Store.pp_store store

  let string_of_opconf = Format.asprintf "%a" pp_opconf
  let string_of_store = OpLang.Store.string_of_store
  let pp_store = OpLang.Store.pp_store

  module Storectx = OpLang.Store.Storectx

  let infer_type_store = OpLang.Store.infer_type_store

  (*

  (* the typing context for an evaluation contexts is a pair (σ,τ),
     with σ the type of the hole and τ the return type *)
     type ectx_typ = OpLang.typ * OpLang.typ

  let ectx_typ_to_string (ty1, ty2) =
    OpLang.string_of_type ty1 ^ " ⇝ " ^ OpLang.string_of_type ty2

  let pp_ectx_typ fmt (ty1, ty2) =
    Format.fprintf fmt "%a ⇝ %a" OpLang.pp_type ty1 OpLang.pp_type ty2

  let ectx_typ_to_yojson etyp = `String (ectx_typ_to_string etyp)
*)
  module Stackctx = Typectx.Make_List_Unit (struct
    type t = OpLang.typ

    let pp = OpLang.pp_type
    let to_yojson = OpLang.typ_to_yojson
  end)

  module UnitNames = Stackctx.Names
  module Names = Names.MakeAggregate (OpLang.Names) (UnitNames)

  let inj_name nn = Either.Left nn

  module Namectx = Typectx.Aggregate (OpLang.Namectx) (Stackctx) (Names)
  module StackRenaming = Renaming.MakeNoName (Stackctx)

  module Renaming =
    Renaming.Aggregate (OpLang.Renaming) (StackRenaming) (Namectx)

  module StackEnv =
    Ienv.Make_Stack
      (StackRenaming)
      (struct
        type t = OpLang.eval_context [@@deriving to_yojson]

        let embed_name () =
          failwith "Embeding names is not implemented for Stacks. Please report"

        let renam_act _renam ectx =
          ectx (*No need to rename as there are no names*)

        let pp = OpLang.pp_eval_context
      end)

  (* Interactive environments γ are pairs formed by partial maps from functional names to functional values,
     and a stack of evaluation contexts. *)

  module IEnv = Ienv.Aggregate (OpLang.IEnv) (StackEnv) (Renaming)

  type abstract_normal_form =
    (OpLang.AVal.abstract_val, unit, OpLang.Names.name, unit) OpLang.Nf.nf_term
    * OpLang.Store.store

  let get_subject_name (a_nf_term, _) =
    let f_fn nn = (nn, Some (inj_name nn)) in
    snd @@ OpLang.Nf.map_fn None f_fn a_nf_term

  let pp_a_nf ~pp_dir fmt (a_nf_term, store) =
    let pp_ectx fmt () = Format.pp_print_string fmt "" in
    let pp_cn fmt () = Format.pp_print_string fmt "ret" in
    let pp_a_nf_term =
      OpLang.Nf.pp_nf_term ~pp_dir OpLang.AVal.pp_abstract_val pp_ectx
        OpLang.Names.pp_name pp_cn in
    if store = OpLang.Store.empty_store then pp_a_nf_term fmt a_nf_term
    else
      Format.fprintf fmt "%a,%a" pp_a_nf_term a_nf_term OpLang.Store.pp_store
        store

  let string_of_a_nf dir =
    let pp_dir fmt = Format.pp_print_string fmt dir in
    Format.asprintf "%a" (pp_a_nf ~pp_dir)

  let abstract_normal_form_to_yojson a_nf = `String (string_of_a_nf "" a_nf)

  let renaming_a_nf (frenaming, _) (a_nf_term, store) =
    let a_nf_term' =
      OpLang.Nf.map
        ~f_val:(fun aval -> OpLang.AVal.rename aval frenaming)
        ~f_fn:Fun.id ~f_cn:Fun.id ~f_ectx:Fun.id a_nf_term in
    (a_nf_term', store)
  (* TODO: Rename also the store*)

  let concretize_a_nf store ienv (a_nf, renaming) =
    let lnamectx = Renaming.dom renaming in
    let (a_nf_term', store') = renaming_a_nf renaming a_nf in
    let ((fname_env, stack_ctx) as ienv') = IEnv.weaken_r ienv lnamectx in
    let f_cn () =
      match StackEnv.get_last stack_ctx with
      | Some (ectx, stack_ctx') -> (ectx, (fname_env, stack_ctx'))
      | None ->
          failwith
            "Error: trying to concretize a returning abstract normal form in \
             an empty stack. Please report" in
    let f_val aval = (OpLang.AVal.subst_pnames fname_env aval, ()) in
    let f_fn fn = (OpLang.IEnv.lookup_exn fname_env fn, ()) in
    let (nf_term, ()) = OpLang.Nf.map_val () f_val a_nf_term' in
    let (nf_term', ()) = OpLang.Nf.map_fn () f_fn nf_term in
    let (nf_term'', ienv'') = OpLang.Nf.map_cn ienv' f_cn nf_term' in
    Util.Debug.print_debug @@ "New Opponent context is "
    ^ Namectx.to_string (IEnv.im ienv'');
    let newstore = OpLang.Store.update_store store store' in
    ((OpLang.refold_nf_term nf_term'', newstore), ienv'')

  let labels_of_a_nf_term =
    OpLang.Nf.apply_val [] OpLang.AVal.labels_of_abstract_val

  let abstracting_store = OpLang.Store.restrict
  (* TODO: Deal with the abstraction process of the heap properly *)

  let abstracting_nf_term nf_term ((fnamectxO, stackctxO) as namectxO) =
    Util.Debug.print_debug @@ "Abstracting_nf_term in the context "
    ^ Namectx.to_string namectxO;
    let ty_out = Stackctx.lookup_exn stackctxO () in
    let inj_ty ty = ty in
    let get_type_fname fn =
      let nty = OpLang.Namectx.lookup_exn fnamectxO fn in
      snd @@ OpLang.get_input_type nty
      (* TODO: we should do something with the tvar_l *) in
    let get_type_cname () = ty_out in
    let nf_typed_term =
      OpLang.type_annotating_val ~inj_ty ~get_type_fname ~get_type_cname nf_term
    in
    let get_type_fname fn =
      let nty = OpLang.Namectx.lookup_exn fnamectxO fn in
      OpLang.get_output_type nty in
    let nf_typed_term' =
      OpLang.type_annotating_ectx ~get_type_fname ty_out nf_typed_term in
    (* We could probably simplify type_annotating_ectx *)
    let f_val (value, nty) = OpLang.AVal.abstracting_value value fnamectxO nty in
    let empty_stack = StackEnv.empty stackctxO in
    let f_ectx (ectx, (ty_hole, _)) =
      let ((), stack) = StackEnv.add_fresh empty_stack "" ty_hole ectx in
      ((), stack) in
    let empty_fname_env = OpLang.IEnv.empty fnamectxO in
    let (a_nf_term, fname_env) =
      OpLang.Nf.map_val empty_fname_env f_val nf_typed_term' in
    let (a_nf_term', stack) = OpLang.Nf.map_ectx empty_stack f_ectx a_nf_term in
    (a_nf_term', (fname_env, stack))

  let abstracting_nf (nf_term, store) namectxO storectx_discl =
    let (a_nf_term, ienv) = abstracting_nf_term nf_term namectxO in
    if OpLang.Nf.is_error a_nf_term then None
    else
      let label_l = labels_of_a_nf_term a_nf_term in
      let storectx = OpLang.Store.infer_type_store store in
      Util.Debug.print_debug @@ "The full store context is "
      ^ OpLang.Store.Storectx.to_string storectx;
      let storectx_discl' = OpLang.Store.restrict_ctx storectx label_l in
      let storectx_discl'' =
        OpLang.Store.Storectx.concat storectx_discl storectx_discl' in
      Util.Debug.print_debug @@ "The new diclosed store context is "
      ^ OpLang.Store.Storectx.to_string storectx_discl'';
      let store_discl = abstracting_store storectx_discl' store in
      Some ((a_nf_term, store_discl), ienv, storectx_discl'')

  (* Notice that the disclosure process is in fact more complex
     since the image of  store_discl might itself has
     labels that becomes diclosed.
     This computation would necessitate an iterative process. *)

  let eval (opconf, namectxO, storectx_discl) =
    let open EvalMonad in
    let* (term', store') = OpLang.normalize_opconf opconf in
    let nf_term = OpLang.get_nf_term term' in
    match abstracting_nf (nf_term, store') namectxO storectx_discl with
    | Some ((a_nf_term, discl_store), ienv, storectx_discl) ->
        let lnamectx = IEnv.dom ienv in
        return
          (((a_nf_term, discl_store), lnamectx, storectx_discl), ienv, store')
    | None -> fail ()

  let fill_abstract_val storectx fnamectxP nf_skeleton =
    let gen_val in_ty =
      (*TODO: We should take into account the type var list*)
      OpLang.AVal.generate_abstract_val storectx fnamectxP in_ty in
    OpLang.Nf.abstract_nf_term_m ~gen_val nf_skeleton

  let generate_a_nf_call storectx ((fnamectxP, _stackctxP) as namectxP) =
    let fnamectxP_pmap = OpLang.Namectx.to_pmap fnamectxP in
    let fnamectxP_split =
      Util.Pmap.filter_map
        (fun (nn, ty) ->
          if OpLang.Names.is_callable nn then
            let (_, in_ty) = OpLang.get_input_type ty in
            Some (nn, (in_ty, OpLang.get_output_type ty))
          else None)
        fnamectxP_pmap in
    let open BranchMonad in
    let* (skel, typ) = OpLang.generate_nf_term_call fnamectxP_split in
    let* (a_nf_term, lfnamectx) = fill_abstract_val storectx fnamectxP skel in
    let* store = OpLang.Store.generate_store storectx in
    let ((), stackctx) = Stackctx.singleton typ in
    Util.Debug.print_debug @@ "Pushing on the stack "
    ^ Stackctx.to_string stackctx;
    return ((a_nf_term, store), (lfnamectx, stackctx), namectxP)

  let[@warning "-8"] generate_a_nf_ret storectx (fnamectxP, stackctxP) =
    let open BranchMonad in
    Util.Debug.print_debug @@ "Generating a_nf ret in stackctxP :" ^ (Stackctx.to_string stackctxP);
    if Stackctx.is_empty stackctxP then fail ()
    else
      let ty_hole = Stackctx.lookup_exn stackctxP () in
      let (Some stackctx') = Stackctx.is_last stackctxP () ty_hole in
      let cnamectx_pmap = Util.Pmap.singleton ((), (ty_hole, ty_hole)) in
      (* We could remove the second ty_hole by simplifying generate_nf_term_ret *)
      let inj_ty ty = ty in
      let* (skel, _typ) = OpLang.generate_nf_term_ret inj_ty cnamectx_pmap in
      let* (a_nf_term, lfnamectx) = fill_abstract_val storectx fnamectxP skel in
      let* store = OpLang.Store.generate_store storectx in
      let namectxP' = (fnamectxP, stackctx') in
      Util.Debug.print_debug @@ "We get the following return :" ^ (string_of_a_nf "" (a_nf_term, store));
      return ((a_nf_term, store), (lfnamectx, Stackctx.empty), namectxP')

  let generate_a_nf storectx namectxP =
    BranchMonad.para_pair
      (generate_a_nf_call storectx namectxP)
      (generate_a_nf_ret storectx namectxP)

  let[@warning "-8"] type_check_a_nf storectx
      ((fnamectxP, stackctxP) as namectxP) ((nf_term, _), (lnamectx, stackctx))
      =
    let inj_ty ty = ty in
    let empty_res = namectxP in
    let get_type_fname fn = OpLang.Namectx.lookup_exn fnamectxP fn in
    let get_type_cname () =
      (Stackctx.lookup_exn stackctxP (), Stackctx.lookup_exn stackctx ()) in
    let type_check_call aval nty =
      let (_, ty_arg) = OpLang.get_input_type nty in
      (*let ty_out' = OpLang.get_output_type nty in*)
      begin
        if
          OpLang.AVal.type_check_abstract_val storectx fnamectxP ty_arg
            (aval, lnamectx)
        then Some namectxP
        else None
      end in
    let type_check_ret aval ty_hole _ty_out =
      match Stackctx.is_last stackctxP () ty_hole with
      | None -> None
      | Some stackctxP' -> begin
          (* We could also check Stackctx.is_last stackctx () ty_out *)
          let namectxP' = (fnamectxP, stackctxP') in
          if
            OpLang.AVal.type_check_abstract_val storectx fnamectxP ty_hole
              (aval, lnamectx)
          then Some namectxP'
          else None
        end in
    OpLang.type_check_nf_term ~inj_ty ~empty_res ~get_type_fname ~get_type_cname
      ~type_check_call ~type_check_ret nf_term

  (* Beware that is_equiv_a_nf does not check the equivalence of
     the store part of abstract normal forms.
     This is needed for the POGS equivalence. *)
  let is_equiv_a_nf _ (_, _) (_, _) = failwith "Not yet implemented"

  let get_typed_ienv lexBuffer_implem lexBuffer_signature =
    let (ienv, store, namectxP, namectxO) =
      OpLang.get_typed_ienv lexBuffer_implem lexBuffer_signature in
    ( (ienv, StackEnv.empty Stackctx.empty),
      store,
      (namectxP, Stackctx.empty),
      (namectxO, Stackctx.empty) )

  let get_typed_opconf nbprog inBuffer =
    let (opconf, ty, namectxO) = OpLang.get_typed_opconf nbprog inBuffer in
    let ((), stackctx) = Stackctx.singleton ty in
    (opconf, (namectxO, stackctx))
end
