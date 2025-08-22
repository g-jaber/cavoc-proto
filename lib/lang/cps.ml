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
    type name =
      | N of OpLang.Names.name
      | CName of CNames.name (*[@@deriving to_yojson]*)

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

  let filter_negative_val = function
    | GVal value -> begin
        match OpLang.filter_negative_val value with
        | Some nval -> Some (IVal nval)
        | None -> None
      end
    | GPairIn _ | GPairOut _ | GPackOut _ -> None

  type interactive_env = (Names.name, negative_val) Util.Pmap.pmap

  let interactive_env_to_yojson ienv =
    let to_string (nn, nval) =
      (Names.string_of_name nn, `String (string_of_negative_val nval)) in
    `Assoc (Util.Pmap.to_list @@ Util.Pmap.map to_string ienv)

  let pp_ienv fmt ienv =
    let pp_sep fmt () = Format.pp_print_string fmt " ⋅ " in
    let pp_pair fmt (n, nval) =
      Format.fprintf fmt "%a ↦ (%a)" Names.pp_name n pp_negative_val nval in
    let pp_ienv_aux = Util.Pmap.pp_pmap ~pp_sep pp_pair in
    Format.fprintf fmt "[%a]" pp_ienv_aux ienv

  let string_of_ienv = Format.asprintf "%a" pp_ienv
  let empty_ienv = Util.Pmap.empty
  let concat_ienv = Util.Pmap.concat

  type typ =
    | GType of OpLang.typ
    | GProd of OpLang.typ * OpLang.typ
    | GExists of OpLang.typevar list * OpLang.typ * OpLang.typ
    | GEmpty
  [@@deriving to_yojson]

  type negative_type = IType of OpLang.negative_type | INeg of OpLang.typ
  [@@deriving to_yojson]

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

        let to_yojson = OpLang.typ_to_yojson
        let pp = OpLang.pp_type
      end)

  module Namectx = struct
    type name = Names.name
    type typ = negative_type
    type t = OpLang.Namectx.t * CNamectx.t [@@deriving to_yojson]

    let empty = (OpLang.Namectx.empty, CNamectx.empty)

    let concat (namectx1, cnamectx1) (namectx2, cnamectx2) =
      ( OpLang.Namectx.concat namectx1 namectx2,
        CNamectx.concat cnamectx1 cnamectx2 )

    let pp fmt (namectx, cnamectx) =
      Format.fprintf fmt "(%a,%a)" OpLang.Namectx.pp namectx CNamectx.pp
        cnamectx

    let to_string = Format.asprintf "%a" pp

    let get_names (namectx, cnamectx) =
      List.map (fun nn -> Names.N nn) (OpLang.Namectx.get_names namectx)
      @ List.map
          (fun nn -> Names.inj_cont_name nn)
          (CNamectx.get_names cnamectx)

    let lookup_exn (namectx, cnamectx) = function
      | Names.CName cn -> INeg (CNamectx.lookup_exn cnamectx cn)
      | Names.N nn -> IType (OpLang.Namectx.lookup_exn namectx nn)

    let add (namectx, cnamectx) nn nty =
      match (nn, nty) with
      | (Names.CName cn, INeg ty) ->
          let cnamectx' = CNamectx.add cnamectx cn ty in
          (namectx, cnamectx')
      | (Names.N nn, IType ty) ->
          let namectx' = OpLang.Namectx.add namectx nn ty in
          (namectx', cnamectx)
      | _ ->
          failwith
            "Trying to add a name of the wrong type in the name context. \
             Pleasse report."

    let mem (namectx, cnamectx) = function
      | Names.N nn -> OpLang.Namectx.mem namectx nn
      | Names.CName cn -> CNamectx.mem cnamectx cn

    let to_pmap (namectx, cnamectx) =
      let namectx_pmap = OpLang.Namectx.to_pmap namectx in
      let namectx_pmap' =
        Util.Pmap.map (fun (nn, ty) -> (Names.N nn, IType ty)) namectx_pmap
      in
      let cnamectx_pmap = CNamectx.to_pmap cnamectx in
      let cnamectx_pmap' =
        Util.Pmap.map
          (fun (cn, ty) -> (Names.inj_cont_name cn, INeg ty))
          cnamectx_pmap in
      Util.Pmap.concat namectx_pmap' cnamectx_pmap'

    let singleton = function
      | INeg ty ->
          let (cn, cnamectx) = CNamectx.singleton ty in
          (Names.inj_cont_name cn, (OpLang.Namectx.empty, cnamectx))
      | IType ty ->
          let (nn, namectx) = OpLang.Namectx.singleton ty in
          (Names.N nn, (namectx, CNamectx.empty))

    let add_fresh (namectx, cnamectx) nty =
      match nty with
      | INeg ty ->
          let (cn, cnamectx') = CNamectx.add_fresh cnamectx ty in
          (Names.inj_cont_name cn, (namectx, cnamectx'))
      | IType ty ->
          let (nn, namectx') = OpLang.Namectx.add_fresh namectx ty in
          (Names.N nn, (namectx', cnamectx))
  end

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

  let embed_value_env = Util.Pmap.map (fun (nn, v) -> (Names.N nn, IVal v))

  let extract_ienv =
    Util.Pmap.filter_map (function
      | (Names.N n, IVal value) -> Some (n, value)
      | (Names.CName _, ICtx _) -> None
      | _ -> failwith "Name of the wrong type. Please report.")

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

  let[@warning "-27"] type_check_nf_term ~empty_res ~name_ctx ~type_check_val nf
      =
    let inj_ty ty = GType ty in
    let get_type_fname fn = Namectx.lookup_exn name_ctx fn in
    let get_type_cname cn =
      match Namectx.lookup_exn name_ctx cn with
      | INeg ty_hole -> (GType ty_hole, GEmpty)
      | IType ty ->
          failwith "Wrong type for a continuation name. Please report." in
    let type_check_call value nty = type_check_val value nty in
    let type_check_ret value ty_hole ty_out =
      match (ty_hole, ty_out) with
      | (GType ty_hole', GEmpty) -> type_check_val value (INeg ty_hole')
      | _ ->
          failwith
            "Error: tring to type an evaluation context with a return type \
             different of ⊥. Please report." in
    OpLang.type_check_nf_term ~inj_ty ~empty_res ~get_type_fname ~get_type_cname
      ~type_check_call ~type_check_ret nf

  let generate_nf_term namectx =
    let inj_ty ty = GType ty in
    let fname_ctx =
      Util.Pmap.filter_map
        (fun (nn, ty) ->
          match nn with
          | Names.N nn ->
              if OpLang.Names.is_callable nn then Some (Names.N nn, (ty, GEmpty))
              else None
          | _ -> None)
        namectx in
    let cname_ctx =
      Util.Pmap.filter_map
        (fun (cn, ty) ->
          if Names.is_cname cn then Some (cn, (ty, GEmpty)) else None)
        namectx in
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
    type interactive_env = (name, negative_val) Util.Pmap.pmap

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

    let infer_type_abstract_val namectxP namectxO gty aval =
      match (gty, aval) with
      | (GType ty, AVal aval) -> begin
          match
            OpLang.AVal.infer_type_abstract_val
              (extract_name_ctx namectxP)
              (extract_name_ctx namectxO)
              ty aval
          with
          | None -> None
          | Some lnamectx -> Some (embed_name_ctx lnamectx)
        end
      | (GProd (ty, tyhole), APair (aval, cn)) ->
          let nn = Names.inj_cont_name cn in
          if Namectx.mem namectxP nn || Namectx.mem namectxO nn then None
            (* the name cn has to be fresh for the abstract value to be well-typed *)
          else begin
            match
              OpLang.AVal.infer_type_abstract_val
                (extract_name_ctx namectxP)
                (extract_name_ctx namectxO)
                ty aval
            with
            | None -> None
            | Some lnamectx ->
                Some (Namectx.add (embed_name_ctx lnamectx) nn (INeg tyhole))
          end
      | _ -> None

    let abstracting_value gval gty =
      match (gval, gty) with
      | (GPairIn (value, ectx), GProd (ty_v, ty_c)) ->
          let (aval, val_env, lnamectx) =
            OpLang.AVal.abstracting_value value ty_v in
          let (cn, cnamectx) = CNamectx.singleton ty_c in
          let ienv = embed_value_env val_env in
          let ienv' = Util.Pmap.add (Names.inj_cont_name cn, ICtx ectx) ienv in
          let lnamectx' = (lnamectx, cnamectx) in
          (APair (aval, cn), ienv', lnamectx')
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
    let generate_abstract_val storectx namectxP_pmap gtype =
      let namectxP_pmap' =
        Util.Pmap.filter_map
          (function (Names.N nn, IType nty) -> Some (nn, nty) | _ -> None)
          namectxP_pmap in
      let open OpLang.AVal.BranchMonad in
      match gtype with
      | GType ty ->
          let* (aval, lnamectx) =
            OpLang.AVal.generate_abstract_val storectx namectxP_pmap' ty in
          return (AVal aval, embed_name_ctx lnamectx)
      | GProd (ty, tyhole) ->
          let* (aval, lnamectx) =
            OpLang.AVal.generate_abstract_val storectx namectxP_pmap' ty in
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
            OpLang.AVal.generate_abstract_val storectx namectxP_pmap' ty' in
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

    let subst_names (ienv : interactive_env) aval =
      let ienv' = extract_ienv ienv in
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
