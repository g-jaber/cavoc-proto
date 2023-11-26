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

  (* In CPS, the eval contexts are embeded in values,
     so that we collapse the type eval_context to unit *)
  type eval_context = unit

  let string_of_eval_context () = ""

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

  let embed_eval_context _ =
    failwith
      "Embedding an evaluation context is impossible in CPS mode. Please \
       report."

  type interactive_env = (OpLang.Name.name, negative_val) Util.Pmap.pmap

  let string_of_interactive_env =
    Util.Pmap.string_of_pmap "ε" "↪" OpLang.Name.string_of_name
      string_of_negative_val

  let empty_ienv = Util.Pmap.empty
  let concat_ienv = Util.Pmap.concat

  open Nf

  let get_kind_nf (NTerm (cn, term)) =
    match OpLang.get_kind_nf term with
    | NFCallback (fn, value, ectx) ->
        NFCallback (fn, GPairIn (value, NCtx (cn, ectx)), ())
    | NFValue (_, value) -> NFValue (cn, GVal value)
    | NFError _ -> NFError cn
    | NFRaise (_, value) -> NFRaise (cn, GVal value)

  let refold_kind_nf_cps = function
    | NFCallback (IVal nval, gval, ()) -> begin
        match gval with
        | GPairOut (value, cn) | GPackOut (_, value, cn) ->
            (cn, NFCallback (nval, value, ()))
        | _ ->
            failwith @@ "Error: trying to apply a value " ^ string_of_value gval
            ^ " to a callback. Please report."
      end
    | NFValue (ICtx (NCtx (cn, ectx)), GVal value) ->
        (cn, NFValue (OpLang.embed_eval_context ectx, value))
    | NFError (ICtx (NCtx (cn, ectx))) ->
        (cn, NFError (OpLang.embed_eval_context ectx))
    | NFRaise (ICtx (NCtx (cn, ectx)), GVal value) ->
        (cn, NFRaise (OpLang.embed_eval_context ectx, value))
    | _ -> failwith "Refolding impossible"

  let refold_kind_nf nf =
    let (cn, nf') = refold_kind_nf_cps nf in
    let term = OpLang.refold_kind_nf nf' in
    NTerm (cn, term)
  (*end

    module MakeType (OpLang : Language.TYPED) :
      Language.TYPED with module Name = OpLang.Name = struct
      include MakeBasic (OpLang)*)

  type typ =
    | GType of OpLang.typ
    | GProd of OpLang.typ * OpLang.typ
    | GExists of OpLang.typevar list * OpLang.typ * OpLang.typ
    | GEmpty

  type negative_type = IType of OpLang.negative_type | INeg of OpLang.typ
  type typevar = OpLang.typevar
  type typename = OpLang.typename

  let string_of_type = function
    | GType typ -> OpLang.string_of_type typ
    | GExists (_, typ1, typ2) | GProd (typ1, typ2) ->
        OpLang.string_of_type typ1 ^ "* ¬" ^ OpLang.string_of_type typ2
    | GEmpty -> "⊥"

  let string_of_typename = OpLang.string_of_typename

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
  let embed_name_ctx = embed_name_ctx
  let empty_name_ctx = Util.Pmap.empty
  let concat_name_ctx = Util.Pmap.concat
  let get_names_from_name_ctx = Util.Pmap.dom
  let exception_type = GType OpLang.exception_type

  let string_of_name_ctx =
    Util.Pmap.string_of_pmap "ε" "::" OpLang.Name.string_of_name
      string_of_negative_type

  let generate_typename_subst tvar_l =
    let (typename_l, tvar_ctx) = OpLang.generate_typename_subst tvar_l in
    let tvar_ctx' = Util.Pmap.map_im (fun ty -> GType ty) tvar_ctx in
    (typename_l, tvar_ctx')

  let apply_type_subst typ tvar_subst =
    let tvar_subst =
      Util.Pmap.filter_map_im
        (function GType ty -> Some ty | _ -> None)
        tvar_subst in
    match typ with
    | GType typ -> GType (OpLang.apply_type_subst typ tvar_subst)
    | GExists (tvar_l, typ1, typ2) ->
        (*TODO: we should take into account the fact that
           tvar_l may bind some of the type variables
           of the domain of tvar_subst *)
        let typ1' = OpLang.apply_type_subst typ1 tvar_subst in
        let typ2' = OpLang.apply_type_subst typ2 tvar_subst in
        GExists (tvar_l, typ1', typ2')
    | GProd (typ1, typ2) ->
        let typ1' = OpLang.apply_type_subst typ1 tvar_subst in
        let typ2' = OpLang.apply_type_subst typ2 tvar_subst in
        GProd (typ1', typ2')
    | GEmpty -> typ

  module Memory = OpLang.Memory

  type opconf = term * Memory.memory

  let normalize_opconf (NTerm (cn, term), memory) =
    match OpLang.normalize_opconf (term, memory) with
    | Some (nfterm, memory') -> Some (NTerm (cn, nfterm), memory')
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
    let (int_env, memory, namectxP, namectxO) =
      OpLang.get_typed_interactive_env inBuffer_implem inBuffer_signature in
    ( embed_value_env int_env,
      memory,
      embed_name_ctx @@ namectxP,
      embed_name_ctx @@ namectxO )

  type negative_type_temp = negative_type
  type value_temp = value
  type typ_temp = typ
  type negative_val_temp = negative_val

  module AVal :
    Abstract_val.AVAL_NEG
      with type name = Name.name
       and type value = value_temp
       and type negative_val = negative_val_temp
       and type typ = typ_temp
       and type typevar = OpLang.typevar
       and type negative_type = negative_type_temp
       and module M = Memory.M = struct
    type name = Name.name
    type value = value_temp
    type negative_val = negative_val_temp
    type typ = typ_temp
    type typevar = OpLang.typevar
    type negative_type = negative_type_temp

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
    let generate_abstract_val lnamectx gtype =
      let lnamectx' = extract_name_ctx lnamectx in
      match gtype with
      | GType ty ->
          let* (aval, lnamectx) =
            OpLang.AVal.generate_abstract_val lnamectx' ty in
          return (AVal aval, embed_name_ctx lnamectx)
      | GProd (ty, tyhole) ->
          let* (aval, lnamectx) =
            OpLang.AVal.generate_abstract_val lnamectx' ty in
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
            OpLang.AVal.generate_abstract_val lnamectx' ty' in
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

    (* The function negating_type extract from an interactive type the type of the input arguments
       expected to interact over this type. *)
    let negating_type = function
      | IType ty ->
          let (tvar_l, inp_ty) = OpLang.AVal.get_input_type ty in
          let out_ty = OpLang.AVal.get_output_type ty in
          begin
            match tvar_l with
            | [] -> GProd (inp_ty, out_ty)
            | _ -> GExists (tvar_l, inp_ty, out_ty)
          end
      | INeg ty -> GType ty
  end
end