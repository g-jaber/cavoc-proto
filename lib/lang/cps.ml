(* This functor transform a module OpLang of signature Language.WITHAVAL_INOUT
   into a module of signature Language.WITHAVAL_NEG.
   This is done by introducing named terms and named evaluation contexts,
   and by embedding named evaluation contexts in values. *)
module MakeComp (OpLang : Language.WITHAVAL_INOUT) () : Language.WITHAVAL_NEG =
struct
  module EvalMonad = OpLang.EvalMonad
  open EvalMonad
  (* *)

  (* We consider continuation names, also called covariables in the λμ-calculus *)

  module Mode : Names.MODE = struct
    let is_callable = true let is_cname = true
  end

  module Prefix : Names.PREFIX = struct let prefix = "c" end
  module CNames : Names.NAMES_GEN = Names.MakeGen (Mode) (Prefix) ()

  module Names = struct
    type name = N of OpLang.Names.name | CName of CNames.name
    [@@deriving to_yojson]

    let pp_name fmt = function
      | N nn -> OpLang.Names.pp_name fmt nn
      | CName cn -> CNames.pp_name fmt cn

    let inj_cont_name cn = CName cn
    let string_of_name = Format.asprintf "%a" pp_name

    let is_callable = function
      | N nn -> OpLang.Names.is_callable nn
      | CName _ -> true

    let is_cname = function N _ -> false | CName _ -> true
  end

  type term = NTerm of (CNames.name * OpLang.term)

  let pp_term fmt (NTerm (cn, term)) =
    Format.fprintf fmt "[%a]%a" CNames.pp_name cn OpLang.pp_term term

  let string_of_term = Format.asprintf "%a" pp_term

  type neval_context = NCtx of (CNames.name * OpLang.eval_context)
  [@@deriving to_yojson]

  let pp_neval_context fmt (NCtx (cn, ectx)) =
    Format.fprintf fmt "[%a]%a" CNames.pp_name cn OpLang.pp_eval_context ectx

  (*We refine the type of values to allow pairs (V,E) and (V,c) *)
  type value =
    | GVal of OpLang.value
    | GPairIn of OpLang.value * neval_context
    | GPairOut of OpLang.value * CNames.name
    | GPackOut of OpLang.typename list * OpLang.value * CNames.name
  (* Since we are in Curry-style, we could merge GPairOut and GPackOut *)

  let pp_value fmt = function
    | GVal value -> OpLang.pp_value fmt value
    | GPairIn (value, nctx) ->
        Format.fprintf fmt "(%a,%a)" OpLang.pp_value value pp_neval_context nctx
    | GPairOut (value, cn) | GPackOut (_, value, cn) ->
        Format.fprintf fmt "(%a,%a)" OpLang.pp_value value CNames.pp_name cn

  let string_of_value = Format.asprintf "%a" pp_value

  type negative_val = IVal of OpLang.negative_val | ICtx of neval_context

  let pp_negative_val fmt = function
    | IVal value -> OpLang.pp_negative_val fmt value
    | ICtx (NCtx (cn, ectx)) ->
        Format.fprintf fmt "[%a]%a" CNames.pp_name cn OpLang.pp_eval_context
          ectx

  let string_of_negative_val = Format.asprintf "%a" pp_negative_val
  let negative_val_to_yojson nval = `String (string_of_negative_val nval)

  let filter_negative_val = function
    | GVal value -> begin
        match OpLang.filter_negative_val value with
        | Some nval -> Some (IVal nval)
        | None -> None
      end
    | GPairIn _ | GPairOut _ | GPackOut _ -> None

  module CIEnv =
    Ienv.Make_PMAP
      (CNames)
      (struct
        type t = neval_context [@@deriving to_yojson]

        let pp = pp_neval_context
      end)

  module IEnv =
    Ienv.AggregateDisjoint (OpLang.IEnv) (CIEnv)
      (struct
        type t = Names.name

        let embed1 nn = Names.N nn
        let embed2 cn = Names.CName cn
        let extract1 = function Names.N nn -> Some nn | Names.CName _ -> None
        let extract2 = function Names.CName cn -> Some cn | Names.N _ -> None
      end)
      (struct
        type t = negative_val [@@deriving to_yojson]

        let embed1 v = IVal v
        let embed2 v = ICtx v
        let extract1 = function IVal v -> Some v | ICtx _ -> None
        let extract2 = function ICtx v -> Some v | IVal _ -> None
      end)

  let embed_value_env valenv = (valenv, CIEnv.empty)

  type typ =
    | GType of OpLang.typ
    | GProd of OpLang.typ * OpLang.typ
    | GExists of OpLang.typevar list * OpLang.typ * OpLang.typ
    | GEmpty
  [@@deriving to_yojson]

  type negative_type = IType of OpLang.negative_type | INeg of OpLang.typ

  let negative_type_to_yojson = function
    | IType ntype -> OpLang.negative_type_to_yojson ntype
    | INeg typ -> `String ("¬" ^ OpLang.string_of_type typ)

  let pp_type fmt = function
    | GType typ -> OpLang.pp_type fmt typ
    | GExists (tvar_l, typ1, typ2) ->
        Format.fprintf fmt "%a. %a × ¬%a" OpLang.pp_tvar_l tvar_l OpLang.pp_type
          typ1 OpLang.pp_type typ2
    | GProd (typ1, typ2) ->
        Format.fprintf fmt "%a × ¬%a" OpLang.pp_type typ1 OpLang.pp_type typ2
    | GEmpty -> Format.fprintf fmt "⊥"

  let string_of_type = Format.asprintf "%a" pp_type

  let get_negative_type = function
    | GType typ -> begin
        match OpLang.get_negative_type typ with
        | Some ntyp -> Some (IType ntyp)
        | None -> None
      end
    | GProd _ | GExists _ | GEmpty -> None

  let pp_negative_type fmt = function
    | IType ty -> OpLang.pp_negative_type fmt ty
    | INeg ty -> Format.fprintf fmt "¬(%a)" OpLang.pp_type ty

  let string_of_negative_type = Format.asprintf "%a" pp_negative_type

  module CNamectx =
    Typectx.Make_PMAP
      (CNames)
      (struct
        type t = OpLang.typ

        let to_yojson typ = `String ("¬" ^ OpLang.string_of_type typ)
        let pp fmt typ = Format.fprintf fmt "¬(%a)" OpLang.pp_type typ
      end)

  module Namectx =
    Typectx.AggregateDisjoint (OpLang.Namectx) (CNamectx)
      (struct
        type t = Names.name

        let embed1 nn = Names.N nn
        let embed2 cn = Names.CName cn
        let extract1 = function Names.N nn -> Some nn | Names.CName _ -> None
        let extract2 = function Names.CName cn -> Some cn | Names.N _ -> None
      end)
      (struct
        type t = negative_type [@@deriving to_yojson]

        let embed1 ty = IType ty
        let embed2 ty = INeg ty
        let extract1 = function IType ty -> Some ty | INeg _ -> None
        let extract2 = function INeg ty -> Some ty | IType _ -> None
      end)

  let extract_name_ctx (namectx, _) = namectx
  let embed_name_ctx namectx = (namectx, CNamectx.empty)

  module Store = OpLang.Store

  type opconf = term * Store.store

  let pp_opconf fmt (term, store) =
    Format.fprintf fmt "@[(@[Computation: %a@] @| @[Store: %a@])@]" pp_term term
      Store.pp_store store

  let normalize_opconf (NTerm (cn, term), store) =
    let* (nf_term, store') = OpLang.normalize_opconf (term, store) in
    return (NTerm (cn, nf_term), store')

  let get_typed_opconf nbprog inBuffer =
    let ((term, store), typ, namectxO) =
      OpLang.get_typed_opconf nbprog inBuffer in
    let (cn, cnamectx) = CNamectx.singleton typ in
    let nterm = NTerm (cn, term) in
    let namectxO' = (namectxO, cnamectx) in
    ((nterm, store), GEmpty, namectxO')

  let get_typed_ienv lexBuffer_implem lexBuffer_signature =
    let (int_env, store, namectxP, namectxO) =
      OpLang.get_typed_ienv lexBuffer_implem lexBuffer_signature in
    ( embed_value_env int_env,
      store,
      embed_name_ctx @@ namectxP,
      embed_name_ctx @@ namectxO )

  module Nf :
    Language.NF
      with type ('value, 'ectx, 'fname, 'cname) nf_term =
        ('value, 'ectx, 'fname, 'cname) OpLang.Nf.nf_term
       and module BranchMonad = OpLang.AVal.BranchMonad =
    OpLang.Nf

  let type_annotating_val get_ty =
    let inj_ty ty = GType ty in
    let get_type_fname = get_ty in
    let get_type_cname = get_ty in
    OpLang.type_annotating_val ~inj_ty ~get_type_fname ~get_type_cname

  let conf_type = GEmpty

  let[@warning "-27"] type_check_nf_term ~name_ctx ~type_check_val nf =
    let inj_ty ty = GType ty in
    let empty_res = name_ctx in
    let get_type_fname fn = Namectx.lookup_exn name_ctx fn in
    let get_type_cname cn =
      match Namectx.lookup_exn name_ctx cn with
      | INeg ty_hole -> (GType ty_hole, GEmpty)
      | IType ty ->
          failwith "Wrong type for a continuation name. Please report." in
    let type_check_call value nty =
      if type_check_val value nty then Some name_ctx else None in
    let type_check_ret value ty_hole ty_out =
      match (ty_hole, ty_out) with
      | (GType ty_hole', GEmpty) ->
          if type_check_val value (INeg ty_hole') then Some name_ctx else None
      | _ ->
          failwith
            "Error: tring to type an evaluation context with a return type \
             different of ⊥. Please report." in
    OpLang.type_check_nf_term ~inj_ty ~empty_res ~get_type_fname ~get_type_cname
      ~type_check_call ~type_check_ret nf

  let generate_nf_term namectx_pmap =
    let inj_ty ty = GType ty in
    let fname_ctx =
      Util.Pmap.filter_map
        (fun (nn, ty) ->
          match nn with
          | Names.N nn ->
              if OpLang.Names.is_callable nn then Some (Names.N nn, (ty, GEmpty))
              else None
          | _ -> None)
        namectx_pmap in
    let cname_ctx =
      Util.Pmap.filter_map
        (fun (cn, ty) ->
          if Names.is_cname cn then Some (cn, (ty, GEmpty)) else None)
        namectx_pmap in
    (* For both, the type provided must be ⊥, but we do not check it.*)
    let open OpLang.AVal.BranchMonad in
    let* (a, _) =
      para_pair
        (OpLang.generate_nf_term_call fname_ctx)
        (OpLang.generate_nf_term_ret inj_ty cname_ctx) in
    return a

  (* The function negating_type extract from an interactive type the type of the input arguments
     expected to interact over this type. *)
  let negating_type = function
    | IType ty ->
        Util.Debug.print_debug
          ("Negating the type " ^ OpLang.string_of_negative_type ty);
        let (tvar_l, inp_ty) = OpLang.get_input_type ty in
        let out_ty = OpLang.get_output_type ty in
        begin
          match tvar_l with
          | [] -> GProd (inp_ty, out_ty)
          | _ -> GExists (tvar_l, inp_ty, out_ty)
        end
    | INeg ty -> GType ty

  type normal_form_term = (value, unit, Names.name, Names.name) Nf.nf_term

  let insert_cn cn nf_term =
    let f_cn () = Names.inj_cont_name cn in
    let f_fn fn = Names.N fn in
    let f_val value = value in
    let f_ectx ectx = NCtx (cn, ectx) in
    OpLang.Nf.map ~f_cn ~f_fn ~f_val ~f_ectx nf_term

  let get_nf_term (NTerm (cn, term)) =
    let nf_term = insert_cn cn @@ OpLang.get_nf_term term in
    let f_ret v = GVal v in
    let f_call (v, e) = GPairIn (v, e) in
    OpLang.Nf.merge_val_ectx ~f_call ~f_ret nf_term

  let refold_nf_term nf_term =
    let empty_res = None in
    let[@warning "-8"] f_val = function
      | GPairOut (value, cn) -> (value, Some cn)
      | GPackOut (_, value, cn) -> (value, Some cn)
      | GVal value -> (value, None) in
    let (nf_term', cn_opt) = Nf.map_val empty_res f_val nf_term in
    let[@warning "-8"] f_cn = function
      | ICtx (NCtx (cn, ectx)) -> (ectx, Some cn) in
    let (nf_term'', cn_opt') = Nf.map_cn empty_res f_cn nf_term' in
    let[@warning "-8"] f_fn = function IVal nval -> (nval, None) in
    let (nf_term''', _) = Nf.map_fn empty_res f_fn nf_term'' in
    let term = OpLang.refold_nf_term nf_term''' in
    match (cn_opt, cn_opt') with
    | (Some cn, None) | (None, Some cn) -> NTerm (cn, term)
    | (None, None) ->
        failwith
          "Error: no continuation name can be extracted during the cps. Please \
           report"
    | (Some _, Some _) ->
        failwith
          "Error: two continuation names can be extracted during the cps. \
           Please report"

  type negative_type_temp = negative_type
  type value_temp = value
  type typ_temp = typ
  type negative_val_temp = negative_val

  module AVal :
    Abstract_val.AVAL
      with type name = Names.name
       and type value = value_temp
       and type negative_val = negative_val_temp
       and type typ = typ_temp
       and type negative_type = negative_type_temp
       and type label = Store.label
       and type store_ctx = Store.Storectx.t
       and type name_ctx = Namectx.t
       and type interactive_env = IEnv.t
       and module BranchMonad = Store.BranchMonad = struct
    type name = Names.name
    type label = OpLang.AVal.label
    type value = value_temp
    type negative_val = negative_val_temp
    type typ = typ_temp
    type negative_type = negative_type_temp
    type store_ctx = Store.Storectx.t

    (*    type negative_type = OpLang.negative_type*)
    type name_ctx = Namectx.t
    type interactive_env = IEnv.t

    type abstract_val =
      | AVal of OpLang.AVal.abstract_val
      | APair of OpLang.AVal.abstract_val * CNames.name
      | APack of OpLang.typename list * OpLang.AVal.abstract_val * CNames.name

    let pp_abstract_val fmt = function
      | AVal aval -> OpLang.AVal.pp_abstract_val fmt aval
      | APair (aval, cn) ->
          Format.fprintf fmt "(%a,%a)" OpLang.AVal.pp_abstract_val aval
            CNames.pp_name cn
      | APack (tname_l, aval, cn) ->
          let string_l =
            String.concat "," @@ List.map OpLang.string_of_typename tname_l
            (*TODO: introduce a pp_tname_l pretty printer*) in
          Format.fprintf fmt "(%s,%a,%a)" string_l OpLang.AVal.pp_abstract_val
            aval CNames.pp_name cn

    let string_of_abstract_val = Format.asprintf "%a" pp_abstract_val

    let names_of_abstract_val = function
      | AVal aval ->
          List.map
            (fun nn -> Names.N nn)
            (OpLang.AVal.names_of_abstract_val aval)
      | APair (aval, cn) | APack (_, aval, cn) ->
          let names_l =
            List.map
              (fun nn -> Names.N nn)
              (OpLang.AVal.names_of_abstract_val aval) in
          Names.inj_cont_name cn :: names_l

    let labels_of_abstract_val = function
      | AVal aval | APair (aval, _) | APack (_, aval, _) ->
          OpLang.AVal.labels_of_abstract_val aval

    let type_check_abstract_val namectxP namectxO gty
        (aval, (lfnamectx, lcnamectx)) =
      match (gty, aval) with
      | (GType ty, AVal aval) ->
          CNamectx.is_empty lcnamectx
          && OpLang.AVal.type_check_abstract_val
               (extract_name_ctx namectxP)
               (extract_name_ctx namectxO)
               ty (aval, lfnamectx)
      | (GProd (ty, tyhole), APair (aval, cn)) ->
          CNamectx.is_singleton lcnamectx cn tyhole
          && OpLang.AVal.type_check_abstract_val
               (extract_name_ctx namectxP)
               (extract_name_ctx namectxO)
               ty (aval, lfnamectx)
      | _ -> false

    let abstracting_value gval gty =
      match (gval, gty) with
      | (GPairIn (value, ectx), GProd (ty_v, ty_c)) ->
          let (aval, val_env, lnamectx) =
            OpLang.AVal.abstracting_value value ty_v in
          let (cn, cnamectx) = CNamectx.singleton ty_c in
          let cienv = CIEnv.add_last_check CIEnv.empty cn ectx in
          let lnamectx' = (lnamectx, cnamectx) in
          (APair (aval, cn), (val_env, cienv), lnamectx')
      | (GVal value, GType ty) ->
          let (aval, val_env, lnamectx) =
            OpLang.AVal.abstracting_value value ty in
          let ienv = embed_value_env val_env in
          let lnamectx = embed_name_ctx lnamectx in
          (AVal aval, ienv, lnamectx)
      | (_, _) -> failwith "Ill-typed interactive value. Please report."

    module BranchMonad = OpLang.AVal.BranchMonad

    (* From the list-presented interactive name context Γ_P and a glue type τ,
       we generate all the possible pairs (A,Δ) such that
       Γ_P;_ ⊢ A : τ ▷ Δ
       Freshness of names that appear in Δ is guaranteed by a gensym, so that we do not need to provide Γ_O. *)
    let generate_abstract_val storectx (namectx, _) gtype =
      let open OpLang.AVal.BranchMonad in
      match gtype with
      | GType ty ->
          let* (aval, lnamectx) =
            OpLang.AVal.generate_abstract_val storectx namectx ty in
          return (AVal aval, embed_name_ctx lnamectx)
      | GProd (ty, tyhole) ->
          let* (aval, lnamectx) =
            OpLang.AVal.generate_abstract_val storectx namectx ty in
          let (cn, cnamectx) = CNamectx.singleton tyhole in
          let lnamectx' = (lnamectx, cnamectx) in
          return (APair (aval, cn), lnamectx')
      | GExists (tvar_l, ty, tyhole) ->
          Util.Debug.print_debug
            "Generating an abstract value for an existential type";
          let (tname_l, type_subst) = OpLang.generate_typename_subst tvar_l in
          let ty' = OpLang.apply_type_subst ty type_subst in
          let tyhole' = OpLang.apply_type_subst tyhole type_subst in
          let* (aval, lnamectx) =
            OpLang.AVal.generate_abstract_val storectx namectx ty' in
          let (cn, cnamectx) = CNamectx.singleton tyhole' in
          let lnamectx' = (lnamectx, cnamectx) in
          return (APack (tname_l, aval, cn), lnamectx')
      | _ -> failwith "The glue type is not valid. Please report."

    let unify_abstract_val _nspan _aval1 _aval2 =
      failwith "To be reimplemented."
    (*      match (aval1, aval2) with
      | (APair (aval1, cn1), APair (aval2, cn2)) ->
          let nspan1_option = OpLang.AVal.unify_abstract_val nspan aval1 aval2 in
          begin
            match nspan1_option with
            | None -> None
            | Some nspan1 ->
                Util.Namespan.add_nspan
                  (Names.inj_cont_name cn1, Names.inj_cont_name cn2)
                  nspan1
          end
      | (AVal aval1, AVal aval2) ->
          OpLang.AVal.unify_abstract_val nspan aval1 aval2
      | _ -> None*)

    let subst_names ((val_env, _) : interactive_env) aval =
      match aval with
      | AVal aval -> GVal (OpLang.AVal.subst_names val_env aval)
      | APair (aval, cn) ->
          let value = OpLang.AVal.subst_names val_env aval in
          GPairOut (value, cn)
      | APack (tname_l, aval, cn) ->
          let value = OpLang.AVal.subst_names val_env aval in
          GPackOut (tname_l, value, cn)
  end
end
