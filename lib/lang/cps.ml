module type WITHNUP = sig
  include Language.WITHNUP include Names.CONT_NAMES with type name := name
end

module Make (OpLang : WITHNUP) = struct
  (*instantiating*)
  include OpLang
  (* *)

  module Memory = OpLang.Memory

  type named_ectx = NCtx of (OpLang.cont_name * OpLang.eval_ctx)

  type glue_val =
    | GVal of OpLang.value
    | GPair of OpLang.value * named_ectx
    | GRaise of OpLang.value

  type interactive_val = IVal of OpLang.value | ICtx of named_ectx

  type abstract_val =
    | ANup of OpLang.Nup.nup
    | APair of (OpLang.Nup.nup * OpLang.cont_name)
    | AExists of (OpLang.typename list * abstract_val)
    | ARaise of OpLang.Nup.nup

  let rec string_of_abstract_val = function
    | ANup nup -> OpLang.Nup.string_of_nup nup
    | APair (nup, cn) ->
        "("
        ^ OpLang.Nup.string_of_nup nup
        ^ ","
        ^ OpLang.string_of_cont_name cn
        ^ ")"
    | AExists (tname_l, aval) ->
        let string_l = List.map OpLang.string_of_typename tname_l in
        "(" ^ String.concat "," string_l ^ ","
        ^ string_of_abstract_val aval
        ^ ")"
    | ARaise nup -> "raise " ^ OpLang.Nup.string_of_nup nup

  let rec names_of_abstract_val = function
    | ANup nup | ARaise nup -> OpLang.Nup.names_of_nup nup
    | APair (nup, cn) -> OpLang.inj_cont_name cn :: OpLang.Nup.names_of_nup nup
    | AExists (_, aval) ->
        names_of_abstract_val aval (* We should also add the type names !*)

  let string_of_interactive_val = function
    | IVal value -> OpLang.string_of_value value
    | ICtx (NCtx (cn, ectx)) ->
        "["
        ^ OpLang.string_of_cont_name cn
        ^ "]"
        ^ OpLang.string_of_eval_ctx ectx

  type interactive_type = IType of OpLang.typ | INeg of OpLang.typ

  let string_of_interactive_type = function
    | IType ty -> OpLang.string_of_type ty
    | INeg ty -> "¬" ^ OpLang.string_of_type ty

  type name_type_ctx = (OpLang.name, interactive_type) Util.Pmap.pmap

  let extract_name_ctx =
    Util.Pmap.filter_map (function
      | (n, IType ty) -> Some (n, ty)
      | (_, INeg _) -> None)

  let embed_name_ctx = Util.Pmap.map_im (fun ty -> IType ty)
  let empty_name_type_ctx = Util.Pmap.empty
  let concat_name_type_ctx = Util.Pmap.concat
  let get_names_from_name_type_ctx = Util.Pmap.dom

  let string_of_name_type_ctx =
    Util.Pmap.string_of_pmap "ε" "::" OpLang.string_of_name
      string_of_interactive_type

  type glue_type =
    | GType of OpLang.typ
    | GProd of OpLang.typ * interactive_type
    | GExists of OpLang.typevar list * OpLang.typ * interactive_type

  type computation = NTerm of (OpLang.cont_name * OpLang.term)

  (*We redefine opconf *)
  type opconf = computation * Memory.memory

  let string_of_computation (NTerm (cn, term)) =
    "[" ^ OpLang.string_of_cont_name cn ^ "]" ^ OpLang.string_of_term term

  let generate_computation term ty =
    let cn = OpLang.fresh_cname () in
    (NTerm (cn, term), Util.Pmap.singleton (OpLang.inj_cont_name cn, INeg ty))

  let type_check_abstract_val name_type_ctxP name_type_ctxO gty aval =
    match (gty, aval) with
    | (GType ty, ANup nup) -> begin
        match
          OpLang.Nup.type_check_nup
            (extract_name_ctx name_type_ctxP)
            (extract_name_ctx name_type_ctxO)
            ty nup
        with
        | None -> None
        | Some name_ctx -> Some (embed_name_ctx name_ctx)
      end
    | (GProd (ty, (INeg _ as ity)), APair (nup, cn)) ->
        let nn = OpLang.inj_cont_name cn in
        if Util.Pmap.mem nn name_type_ctxP || Util.Pmap.mem nn name_type_ctxO
        then None
          (* the name cn has to be fresh for the abstract value to be well-typed *)
        else begin
          match
            OpLang.Nup.type_check_nup
              (extract_name_ctx name_type_ctxP)
              (extract_name_ctx name_type_ctxO)
              ty nup
          with
          | None -> None
          | Some name_ctx ->
              Some (Util.Pmap.add (nn, ity) (embed_name_ctx name_ctx))
        end
    | _ -> None

  type interactive_env = (OpLang.name, interactive_val) Util.Pmap.pmap

  let embed_value_env = Util.Pmap.map_im (fun v -> IVal v)

  let extract_val_env =
    Util.Pmap.filter_map (function
      | (n, IVal v) -> Some (n, v)
      | (_, ICtx _) -> None)

  let empty_ienv = Util.Pmap.empty

  let string_of_interactive_env =
    Util.Pmap.string_of_pmap "ε" "↪" OpLang.string_of_name
      string_of_interactive_val

  let trigger_ienv ienv kind = Util.Pmap.lookup kind ienv
  let concat_ienv = Util.Pmap.concat

  let abstract_glue_val gval gty =
    match (gval, gty) with
    | (GPair (value, ectx), GProd (ty_v, ty_c)) ->
        let (nup, val_env, name_ctx) = OpLang.Nup.abstract_val value ty_v in
        let cn = OpLang.fresh_cname () in
        let ienv = embed_value_env val_env in
        let ienv' = Util.Pmap.add (OpLang.inj_cont_name cn, ICtx ectx) ienv in
        let name_type_ctx = embed_name_ctx name_ctx in
        let name_type_ctx' =
          Util.Pmap.add (OpLang.inj_cont_name cn, ty_c) name_type_ctx in
        (APair (nup, cn), ienv', name_type_ctx')
    | (GVal value, GType ty) ->
        let (nup, val_env, name_ctx) = OpLang.Nup.abstract_val value ty in
        let ienv = embed_value_env val_env in
        let name_type_ctx = embed_name_ctx name_ctx in
        (ANup nup, ienv, name_type_ctx)
    | (GRaise value, GType _) ->
        let (nup, val_env, name_ctx) =
          OpLang.Nup.abstract_val value OpLang.exception_type in
        let ienv = embed_value_env val_env in
        let name_type_ctx = embed_name_ctx name_ctx in
        (ARaise nup, ienv, name_type_ctx)
    | (_, _) -> failwith "Ill-typed interactive value. Please report."

  module M = OpLang.Nup.M
  open M

  (* From the interactive name context Γ_P and a glue type τ,
     we generate all the possible pairs (A,Δ) such that
     Γ_P;_ ⊢ A : τ ▷ Δ
     Freshness of names that appear in Δ is guaranteed by a gensym, so that we do not need to provide Γ_O. *)
  let rec generate_abstract_val name_type_ctx gtype =
    let name_ctx = extract_name_ctx name_type_ctx in
    match gtype with
    | GType ty ->
        let* (nup, name_ctx) = OpLang.Nup.generate_nup name_ctx ty in
        return (ANup nup, embed_name_ctx name_ctx)
    | GProd (ty, (INeg _ as ity)) ->
        let* (nup, name_ctx) = OpLang.Nup.generate_nup name_ctx ty in
        let cn = OpLang.fresh_cname () in
        let name_type_ctx' =
          Util.Pmap.add (OpLang.inj_cont_name cn, ity) (embed_name_ctx name_ctx)
        in
        return (APair (nup, cn), name_type_ctx')
    | GExists (tvar_l, ty1, INeg ty2) ->
        Util.Debug.print_debug
          "Generating an abstract value for an existential type";
        let (tname_l, type_subst) = OpLang.generate_typename_subst tvar_l in
        let ty1' = OpLang.apply_type_subst ty1 type_subst in
        let ty2' = OpLang.apply_type_subst ty2 type_subst in
        let* (aval, name_type_ctx) =
          generate_abstract_val name_type_ctx (GProd (ty1', INeg ty2')) in
        return (AExists (tname_l, aval), name_type_ctx)
    | _ -> failwith "The glue type is not valid. Please report."

  type kind_interact = name

  let string_of_kind_interact = string_of_name
  let name_of_kind_interact kind = Some kind

  let is_equiv_kind_interact span kind1 kind2 =
    Util.Namespan.is_in_dom_im (kind1, kind2) span

    (* kind_interact_typing provide a way to type check an interact kind within an interactive name context.
     It returns None if the interactive kind is not well-typed.*)  
  let kind_interact_typing = Util.Pmap.lookup

  let extract_kind_interact namectx =
    Util.Pmap.to_list @@ Util.Pmap.filter_dom OpLang.is_callable namectx

  type normal_form = kind_interact * glue_val

  let compute_nf (NTerm (cn, term), memory) =
    match OpLang.compute_nf (term, memory) with
    | None -> None
    | Some (nf, memory') ->
        let kind_nf =
          begin
            match get_kind_nf nf with
            | NFCallback (fn, value, ectx) ->
                Some (fn, GPair (value, NCtx (cn, ectx)))
            | NFValue value -> Some (OpLang.inj_cont_name cn, GVal value)
            | NFError -> None
            | NFRaise value -> Some (OpLang.inj_cont_name cn, GRaise value)
          end in
        Some (kind_nf, memory')

  let val_composition ienv ival aval =
    match (ival, aval) with
    | (ICtx (NCtx (cn, ectx)), ANup nup) ->
        let value = OpLang.Nup.subst_names_of_nup (extract_val_env ienv) nup in
        NTerm (cn, OpLang.fill_hole ectx value)
    | (IVal fun_val, APair (nup, cn))
    | (IVal fun_val, AExists (_, APair (nup, cn))) ->
        let value = OpLang.Nup.subst_names_of_nup (extract_val_env ienv) nup in
        NTerm (cn, OpLang.apply_value fun_val value)
    | _ ->
        failwith
          ("Error: the interactive value "
          ^ string_of_interactive_val ival
          ^ " cannot be composed with the abstract value "
          ^ string_of_abstract_val aval
          ^ ". Please report")

  (* The function neg_type extract from an interactive type the type of the input arguments
     expected to interact over this type. *)
  let neg_type = function
    | IType ty ->
        let (tvar_l, inp_ty) = OpLang.get_input_type ty in
        let out_ty = OpLang.get_output_type ty in
        begin
          match tvar_l with
          | [] -> GProd (inp_ty, INeg out_ty)
          | _ -> GExists (tvar_l, inp_ty, INeg out_ty)
        end
    | INeg ty -> GType ty

  let abstract_kind (kind, gval) namectxO =
    let ty_option = kind_interact_typing kind namectxO in
    match ty_option with
    | Some ty ->
        let nty = neg_type ty in
        Some (abstract_glue_val gval nty)
    | None -> None

  type abstract_normal_form = kind_interact * abstract_val

  let generate_a_nf namectxP =
    let kind_list = extract_kind_interact namectxP in
    let* (kind, ty) = M.para_list @@ kind_list in
    let* (aval, lnamectx) = generate_abstract_val namectxP (neg_type ty) in
    return ((kind, aval), lnamectx)

  let type_check_a_nf namectxP namectxO (kind, aval) =
    match kind_interact_typing kind namectxP with
    | None -> None
    | Some ty ->
        let dual_ty = neg_type ty in
        type_check_abstract_val namectxP namectxO dual_ty aval

  let unify_abstract_val nspan aval1 aval2 =
    match (aval1, aval2) with
    | (APair (nup1, cn1), APair (nup2, cn2)) ->
        let nspan1_option = OpLang.Nup.unify_nup nspan nup1 nup2 in
        begin
          match nspan1_option with
          | None -> None
          | Some nspan1 ->
              Util.Namespan.add_nspan
                (OpLang.inj_cont_name cn1, OpLang.inj_cont_name cn2)
                nspan1
        end
    | (ANup nup1, ANup nup2) -> OpLang.Nup.unify_nup nspan nup1 nup2
    | _ -> None

  let get_typed_computation nbprog inBuffer =
    let (term, ty, _) = OpLang.get_typed_term nbprog inBuffer in
    generate_computation term ty

  let get_typed_ienv inBuffer_implem inBuffer_signature =
    let (val_env, memory, namectxP, namectxO) =
      OpLang.get_typed_val_env inBuffer_implem inBuffer_signature in
    ( embed_value_env val_env,
      memory,
      embed_name_ctx @@ namectxP,
      embed_name_ctx @@ namectxO )
end

module type INTLANG = sig
  include Interactive.LANG include Names.CONT_NAMES with type name := name
end
