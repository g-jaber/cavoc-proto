module MakeComp (OpLang : Language.WITHAVAL_INOUT) :
  Language.WITHAVAL_NEG with module Name = OpLang.Name = struct
  module Name = OpLang.Name
  (* *)

  (* We consider named terms, as in the λμ-calculus *)
  type term = NTerm of (Name.cont_name * OpLang.term)

  let string_of_term (NTerm (cn, term)) =
    "[" ^ OpLang.Name.string_of_cont_name cn ^ "]" ^ OpLang.string_of_term term

  type neval_context = NCtx of (Name.cont_name * OpLang.eval_context)

  let string_of_neval_context (NCtx (cn, ectx)) =
    "[" ^ Name.string_of_cont_name cn ^ "]" ^ OpLang.string_of_eval_context ectx

  (*We refine the type of values to allow pairs (V,E) and (V,c) *)
  type value =
    | GVal of OpLang.value
    | GPairIn of OpLang.value * neval_context
    | GPairOut of OpLang.value * Name.cont_name
    | GPackOut of OpLang.typename list * OpLang.value * Name.cont_name
  (* Since we are in Curry-style, we could merge GPairOut and GPackOut *)

  let string_of_value = function
    | GVal value -> OpLang.string_of_value value
    | GPairIn (value, nctx) ->
        "("
        ^ OpLang.string_of_value value
        ^ ","
        ^ string_of_neval_context nctx
        ^ ")"
    | GPairOut (value, cn) | GPackOut (_, value, cn) ->
        "("
        ^ OpLang.string_of_value value
        ^ ","
        ^ Name.string_of_cont_name cn
        ^ ")"

  type negative_val = IVal of OpLang.negative_val | ICtx of neval_context

  let filter_negative_val = function
    | GVal value -> begin
        match OpLang.filter_negative_val value with
        | Some nval -> Some (IVal nval)
        | None -> None
      end
    | GPairIn _ | GPairOut _ | GPackOut _ -> None

  let string_of_negative_val = function
    | IVal value -> OpLang.string_of_negative_val value
    | ICtx (NCtx (cn, ectx)) ->
        "["
        ^ Name.string_of_cont_name cn
        ^ "]"
        ^ OpLang.string_of_eval_context ectx

  type interactive_env = (OpLang.Name.name, negative_val) Util.Pmap.pmap

  let string_of_interactive_env =
    Util.Pmap.string_of_pmap "ε" "↪" OpLang.Name.string_of_name
      string_of_negative_val

  let empty_ienv = Util.Pmap.empty
  let concat_ienv = Util.Pmap.concat

  type typ =
    | GType of OpLang.typ
    | GProd of OpLang.typ * OpLang.typ
    | GExists of OpLang.typevar list * OpLang.typ * OpLang.typ
    | GEmpty

  let exception_type = GType OpLang.exception_type

  type negative_type = IType of OpLang.negative_type | INeg of OpLang.typ

  let string_of_type = function
    | GType typ -> OpLang.string_of_type typ
    | GExists (_, typ1, typ2) | GProd (typ1, typ2) ->
        OpLang.string_of_type typ1 ^ "* ¬" ^ OpLang.string_of_type typ2
    | GEmpty -> "⊥"

  let get_negative_type = function
    | GType typ -> begin
        match OpLang.get_negative_type typ with
        | Some ntyp -> Some (IType ntyp)
        | None -> None
      end
    | GProd _ | GExists _ | GEmpty -> None

  let string_of_negative_type = function
    | IType ty -> OpLang.string_of_negative_type ty
    | INeg ty -> "¬" ^ OpLang.string_of_type ty

  type name_ctx = (Name.name, negative_type) Util.Pmap.pmap

  let extract_name_ctx =
    Util.Pmap.filter_map (function
      | (n, IType ty) -> Some (n, ty)
      | (_, INeg _) -> None)

  let embed_name_ctx = Util.Pmap.map_im (fun ty -> IType ty)
  let empty_name_ctx = Util.Pmap.empty
  let concat_name_ctx = Util.Pmap.concat
  let get_names_from_name_ctx = Util.Pmap.dom

  let string_of_name_ctx =
    Util.Pmap.string_of_pmap "ε" "::" OpLang.Name.string_of_name
      string_of_negative_type

  module Store = OpLang.Store

  type opconf = term * Store.store

  let normalize_opconf (NTerm (cn, term), store) =
    match OpLang.normalize_opconf (term, store) with
    | Some (nfterm, store') -> Some (NTerm (cn, nfterm), store')
    | None -> None

  let embed_value_env = Util.Pmap.map_im (fun v -> IVal v)

  let extract_int_env =
    Util.Pmap.filter_map (function
      | (n, IVal value) -> Some (n, value)
      | (_, ICtx _) -> None)

  let get_typed_term nbprog inBuffer =
    let (term, typ, namectxO) = OpLang.get_typed_term nbprog inBuffer in
    let cn = Name.fresh_cname () in
    let nterm = NTerm (cn, term) in
    let namectxO' =
      Util.Pmap.add (Name.inj_cont_name cn, INeg typ) (embed_name_ctx namectxO)
    in
    (nterm, GEmpty, namectxO')

  let get_typed_interactive_env inBuffer_implem inBuffer_signature =
    let (int_env, store, namectxP, namectxO) =
      OpLang.get_typed_interactive_env inBuffer_implem inBuffer_signature in
    ( embed_value_env int_env,
      store,
      embed_name_ctx @@ namectxP,
      embed_name_ctx @@ namectxO )

  module Nf :
    Language.NF
      with type ('value, 'ectx, 'fname, 'cname) nf_term =
        ('value, 'ectx, 'fname, 'cname) OpLang.Nf.nf_term
       and module M = OpLang.AVal.M =
    OpLang.Nf

  (* The function negating_type extract from an interactive type the type of the input arguments
     expected to interact over this type. *)
  let negating_type = function
    | IType ty ->
        let (tvar_l, inp_ty) = OpLang.get_input_type ty in
        let out_ty = OpLang.get_output_type ty in
        begin
          match tvar_l with
          | [] -> GProd (inp_ty, out_ty)
          | _ -> GExists (tvar_l, inp_ty, out_ty)
        end
    | INeg ty -> GType ty

  type normal_form_term = (value, unit, Name.name, Name.name) Nf.nf_term

  let insert_cn cn nf_term =
    let f_cn () = OpLang.Name.inj_cont_name cn in
    let f_fn fn = fn in
    let f_val value = value in
    let f_ectx ectx = NCtx (cn, ectx) in
    OpLang.Nf.map ~f_cn ~f_fn ~f_val ~f_ectx nf_term

  let get_nf_term (NTerm (cn, term)) =
    let nf_term = insert_cn cn @@ OpLang.get_nf_term term in
    let f_val v = GVal v in
    let f_merge (v, e) = GPairIn (v, e) in
    OpLang.Nf.merge_val_ectx ~f_val ~f_merge nf_term

  let extract_cn nf_term =
    let f_call (fn, v, cn) = ((fn, v, ()), cn) in
    let f_ret (NCtx (cn, ectx), v) = ((ectx, v), cn) in
    let f_exn (NCtx (cn, ectx), v) = ((ectx, v), cn) in
    let f_error (NCtx (cn, ectx)) = (ectx, cn) in
    OpLang.Nf.map_cons ~f_call ~f_ret ~f_exn ~f_error nf_term

  let expel_ectx nf_term =
    let f_call (nval, gval, ()) =
      match (nval, gval) with
      | (IVal nval, GPairOut (value, cn)) | (IVal nval, GPackOut (_, value, cn))
        ->
          ((nval, value, cn), ())
      | (_, gval) ->
          failwith @@ "Error: trying to apply a value " ^ string_of_value gval
          ^ " to a callback. Please report." in
    let[@warning "-8"] f_ret (ICtx nctx, GVal value) = ((nctx, value), ()) in
    let f_exn = f_ret in
    let[@warning "-8"] f_error (ICtx nctx) = (nctx, ()) in
    fst @@ OpLang.Nf.map_cons ~f_call ~f_ret ~f_exn ~f_error nf_term

  let refold_nf_term nf_term =
    let nf_term = expel_ectx nf_term in
    let (nf_term, cn) = extract_cn nf_term in
    let term = OpLang.refold_nf_term nf_term in
    NTerm (cn, term)

  type negative_type_temp = negative_type
  type value_temp = value
  type typ_temp = typ
  type negative_val_temp = negative_val

  module AVal :
    Abstract_val.AVAL
      with type name = Name.name
       and type value = value_temp
       and type negative_val = negative_val_temp
       and type typ = typ_temp
       and type negative_type = negative_type_temp
       and type label = Store.label
       and type store_ctx = Store.store_ctx
       and module M = Store.M = struct
    type name = Name.name
    type label = OpLang.AVal.label
    type value = value_temp
    type negative_val = negative_val_temp
    type typ = typ_temp
    type negative_type = negative_type_temp
    type store_ctx = Store.store_ctx

    (*    type negative_type = OpLang.negative_type*)
    type name_ctx = (name, negative_type) Util.Pmap.pmap
    type interactive_env = (name, negative_val) Util.Pmap.pmap

    type abstract_val =
      | AVal of OpLang.AVal.abstract_val
      | APair of OpLang.AVal.abstract_val * Name.cont_name
      | APack of
          OpLang.typename list * OpLang.AVal.abstract_val * Name.cont_name

    let string_of_abstract_val = function
      | AVal aval -> OpLang.AVal.string_of_abstract_val aval
      | APair (aval, cn) ->
          "("
          ^ OpLang.AVal.string_of_abstract_val aval
          ^ ","
          ^ Name.string_of_cont_name cn
          ^ ")"
      | APack (tname_l, aval, cn) ->
          let string_l = List.map OpLang.string_of_typename tname_l in
          "(" ^ String.concat "," string_l ^ ","
          ^ OpLang.AVal.string_of_abstract_val aval
          ^ ","
          ^ Name.string_of_cont_name cn
          ^ ")"

    let names_of_abstract_val = function
      | AVal aval -> OpLang.AVal.names_of_abstract_val aval
      | APair (aval, cn) | APack (_, aval, cn) ->
          Name.inj_cont_name cn :: OpLang.AVal.names_of_abstract_val aval

    let labels_of_abstract_val = function
      | AVal aval | APair (aval, _) | APack (_, aval, _) ->
          OpLang.AVal.labels_of_abstract_val aval

    let type_check_abstract_val namectxP namectxO gty aval =
      match (gty, aval) with
      | (GType ty, AVal aval) -> begin
          match
            OpLang.AVal.type_check_abstract_val
              (extract_name_ctx namectxP)
              (extract_name_ctx namectxO)
              ty aval
          with
          | None -> None
          | Some lnamectx -> Some (embed_name_ctx lnamectx)
        end
      | (GProd (ty, tyhole), APair (aval, cn)) ->
          let nn = Name.inj_cont_name cn in
          if Util.Pmap.mem nn namectxP || Util.Pmap.mem nn namectxO then None
            (* the name cn has to be fresh for the abstract value to be well-typed *)
          else begin
            match
              OpLang.AVal.type_check_abstract_val
                (extract_name_ctx namectxP)
                (extract_name_ctx namectxO)
                ty aval
            with
            | None -> None
            | Some lnamectx ->
                Some (Util.Pmap.add (nn, INeg tyhole) (embed_name_ctx lnamectx))
          end
      | _ -> None

    let abstracting_value gval gty =
      match (gval, gty) with
      | (GPairIn (value, ectx), GProd (ty_v, ty_c)) ->
          let (aval, val_env, lnamectx) =
            OpLang.AVal.abstracting_value value ty_v in
          let cn = Name.fresh_cname () in
          let ienv = embed_value_env val_env in
          let ienv' = Util.Pmap.add (Name.inj_cont_name cn, ICtx ectx) ienv in
          let lnamectx = embed_name_ctx lnamectx in
          let lnamectx' =
            Util.Pmap.add (Name.inj_cont_name cn, INeg ty_c) lnamectx in
          (APair (aval, cn), ienv', lnamectx')
      | (GVal value, GType ty) ->
          let (aval, val_env, lnamectx) =
            OpLang.AVal.abstracting_value value ty in
          let ienv = embed_value_env val_env in
          let lnamectx = embed_name_ctx lnamectx in
          (AVal aval, ienv, lnamectx)
      | (_, _) -> failwith "Ill-typed interactive value. Please report."

    module M = OpLang.AVal.M
    open M

    (* From the interactive name context Γ_P and a glue type τ,
       we generate all the possible pairs (A,Δ) such that
       Γ_P;_ ⊢ A : τ ▷ Δ
       Freshness of names that appear in Δ is guaranteed by a gensym, so that we do not need to provide Γ_O. *)
    let generate_abstract_val storectx lnamectx gtype =
      let lnamectx' = extract_name_ctx lnamectx in
      match gtype with
      | GType ty ->
          let* (aval, lnamectx) =
            OpLang.AVal.generate_abstract_val storectx lnamectx' ty in
          return (AVal aval, embed_name_ctx lnamectx)
      | GProd (ty, tyhole) ->
          let* (aval, lnamectx) =
            OpLang.AVal.generate_abstract_val storectx lnamectx' ty in
          let cn = Name.fresh_cname () in
          let lnamectx' =
            Util.Pmap.add
              (Name.inj_cont_name cn, INeg tyhole)
              (embed_name_ctx lnamectx) in
          return (APair (aval, cn), lnamectx')
      | GExists (tvar_l, ty, tyhole) ->
          Util.Debug.print_debug
            "Generating an abstract value for an existential type";
          let (tname_l, type_subst) = OpLang.generate_typename_subst tvar_l in
          let ty' = OpLang.apply_type_subst ty type_subst in
          let tyhole' = OpLang.apply_type_subst tyhole type_subst in
          let* (aval, lnamectx) =
            OpLang.AVal.generate_abstract_val storectx lnamectx' ty' in
          let cn = Name.fresh_cname () in
          let lnamectx' =
            Util.Pmap.add
              (Name.inj_cont_name cn, INeg tyhole')
              (embed_name_ctx lnamectx) in
          return (APack (tname_l, aval, cn), lnamectx')
      | _ -> failwith "The glue type is not valid. Please report."

    let unify_abstract_val nspan aval1 aval2 =
      match (aval1, aval2) with
      | (APair (aval1, cn1), APair (aval2, cn2)) ->
          let nspan1_option = OpLang.AVal.unify_abstract_val nspan aval1 aval2 in
          begin
            match nspan1_option with
            | None -> None
            | Some nspan1 ->
                Util.Namespan.add_nspan
                  (OpLang.Name.inj_cont_name cn1, OpLang.Name.inj_cont_name cn2)
                  nspan1
          end
      | (AVal aval1, AVal aval2) ->
          OpLang.AVal.unify_abstract_val nspan aval1 aval2
      | _ -> None

    let subst_names ienv aval =
      let ienv' = extract_int_env ienv in
      match aval with
      | AVal aval -> GVal (OpLang.AVal.subst_names ienv' aval)
      | APair (aval, cn) ->
          let value = OpLang.AVal.subst_names ienv' aval in
          GPairOut (value, cn)
      | APack (tname_l, aval, cn) ->
          let value = OpLang.AVal.subst_names ienv' aval in
          GPackOut (tname_l, value, cn)
  end
end
