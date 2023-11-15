module Make (OpLang : Interactive.WITHNUP) = struct
  (*instantiating*)
  type name = OpLang.name
  type value = OpLang.value
  type term = OpLang.term
  type typ = OpLang.typ
  (* *)

  type named_ectx = NCtx of (OpLang.cont_name * OpLang.eval_ctx)
  type glue_val = GVal of OpLang.value | GPair of OpLang.value * named_ectx
  type interactive_val = IVal of OpLang.value | ICtx of named_ectx

  type abstract_val =
    | ANup of OpLang.Nup.nup
    | APair of (OpLang.Nup.nup * OpLang.cont_name)
    | AExists of (OpLang.typename list * abstract_val)

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

  let rec names_of_abstract_val = function
    | ANup nup -> OpLang.Nup.names_of_nup nup
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

  (*let embed_val value = IVal value *)

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

  let string_of_name_type_ctx =
    Util.Pmap.string_of_pmap "ε" "::" OpLang.string_of_name
      string_of_interactive_type

  type glue_type =
    | GType of OpLang.typ
    | GProd of OpLang.typ * interactive_type
    | GExists of OpLang.typevar list * OpLang.typ * interactive_type

  type computation = NTerm of (OpLang.cont_name * OpLang.term)

  let string_of_computation (NTerm (cn, term)) =
    "[" ^ OpLang.string_of_cont_name cn ^ "]" ^ OpLang.string_of_term term

  let generate_computation term ty =
    let cn = OpLang.fresh_cname () in
    (NTerm (cn, term), Util.Pmap.singleton (OpLang.inj_cont_name cn, INeg ty))

  let embed_term (nn, term) =
    match OpLang.get_cont_name nn with
    | Some cn -> NTerm (cn, term)
    | None ->
        failwith @@ "The name " ^ OpLang.string_of_name nn
        ^ "is not a continuation name. Please report."

  let extract_term (NTerm (cn, term)) = (OpLang.inj_cont_name cn, term)

  let rec generate_abstract_val name_type_ctx gtype =
    let name_ctx = extract_name_ctx name_type_ctx in
    match gtype with
    | GType ty ->
        List.map (fun (nup, name_ctx) -> (ANup nup, embed_name_ctx name_ctx))
        @@ OpLang.Nup.generate_nup name_ctx ty
    | GProd (ty, (INeg _ as ity)) ->
        let aux (nup, name_ctx) =
          let cn = OpLang.fresh_cname () in
          let name_type_ctx' =
            Util.Pmap.add
              (OpLang.inj_cont_name cn, ity)
              (embed_name_ctx name_ctx) in
          (APair (nup, cn), name_type_ctx') in
        List.map aux @@ OpLang.Nup.generate_nup name_ctx ty
    | GExists (tvar_l, ty1, INeg ty2) ->
        Util.Debug.print_debug
          "Generating an abstract value for an existential type";
        let (tname_l, type_subst) = OpLang.generate_typename_subst tvar_l in
        let ty1' = OpLang.apply_type_subst ty1 type_subst in
        let ty2' = OpLang.apply_type_subst ty2 type_subst in
        let aux (aval, name_type_ctx) =
          (AExists (tname_l, aval), name_type_ctx) in
        List.map aux
        @@ generate_abstract_val name_type_ctx (GProd (ty1', INeg ty2'))
    | _ -> failwith "The glue type is not valid. Please report."

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
  let singleton_ienv = Util.Pmap.singleton
  let list_to_ienv = Util.Pmap.list_to_pmap

  let string_of_interactive_env =
    Util.Pmap.string_of_pmap "ε" "↪" OpLang.string_of_name
      string_of_interactive_val

  let lookup_ienv = Util.Pmap.lookup
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
    | (_, _) -> failwith "Ill-typed interactive value. Please report."

  let decompose_nf (NTerm (cn, term)) =
    match OpLang.get_callback term with
    | Some (nn, value, ectx) -> Some (nn, GPair (value, NCtx (cn, ectx)))
    | None -> begin
        match OpLang.get_value term with
        | Some value -> Some (OpLang.inj_cont_name cn, GVal value)
        | None ->
            if OpLang.is_error term then None
            else
              failwith
                ("Error: the term " ^ OpLang.string_of_term term
               ^ " is not in canonical form. Please report.")
      end

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
end

module type INTLANG = sig
  include Interactive.LANG include Names.CONT_NAMES with type name := name
end
