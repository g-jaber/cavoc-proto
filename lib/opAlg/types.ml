type rowvar = string
type id = string
type label = string
type opsymbol = string
type typevar = string


(* Types of expressions and operation symbols signatures*)
type vtyp =
  | TUnit
  | TInt
  | TBool
  | TVar of typevar
  | TArrow of vtyp * ctyp
  | TRecord of (label * vtyp) list
  | TProd of vtyp * vtyp
  | TVariant of (label * vtyp) list
  | TForall of (rowvar list * vtyp)
  | TId of id
  | TName of id
  | TUndef

and ctyp = TComp of vtyp * efftyp

and handler_typ = THandler of ctyp * ctyp

and rowpoly = Row of rowvar | Closed
  
and efftyp = { 
  effects: opsymbol list;
  poly: rowpoly }

type cons_sig = 
  | Arity of vtyp * vtyp
  | Tcons of vtyp list * vtyp


let effpure = {effects=[]; poly= Closed}

let rec string_par_of_vtyp = function
  | TUnit -> "Unit"
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow (ty1, ty2) ->
      "(" ^ string_par_of_vtyp ty1 ^ "->" ^ string_of_ctyp ty2 ^ ")"
  | TVar typevar -> typevar
  | TRecord lab_vtyp_l -> "∏(" ^ string_of_sig lab_vtyp_l ^ ")"
  | TVariant lab_vtyp_l -> "Σ(" ^ string_of_sig lab_vtyp_l ^ ")"
  | TProd (ty1, ty2) -> string_par_of_vtyp ty1 ^ "*" ^ string_par_of_vtyp ty2 
  | TForall (rvars, ty) ->
      let rvars_string = String.concat " " rvars in
      "(∀" ^ rvars_string ^ "." ^ string_par_of_vtyp ty ^ ")"
  | TId id -> id
  | TName id -> id
  | TUndef -> "undef"

and string_of_vtyp = function
  | TUnit -> "Unit"
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow (vty, cty) -> string_par_of_vtyp vty ^ "->" ^ string_of_ctyp cty
  | TRecord lab_vtyp_l -> "∏(" ^ string_of_sig lab_vtyp_l ^ ")"
  | TVariant lab_vtyp_l -> "Σ(" ^ string_of_sig lab_vtyp_l ^ ")"
  | TProd (ty1, ty2) -> string_of_vtyp ty1 ^ "*" ^ string_of_vtyp ty2 
  | TVar typevar -> typevar
  | TForall (rvars, vty) ->
      let rvars_string = String.concat " " rvars in
      "∀" ^ rvars_string ^ "." ^ string_par_of_vtyp vty
  | TId id -> id
  | TName id -> id
  | TUndef -> "undef"

and string_of_ctyp = function 
  TComp (ty, ety) -> string_of_vtyp ty ^ "!" ^ string_of_efftyp ety 

and string_of_efftyp ety =
  let row = List.fold_left (fun acc el -> acc ^ ", " ^ el) "" ety.effects in
  let rowvar = match ety.poly with Closed -> "" | Row var -> "| " ^ var in
  "{"^ row ^ rowvar ^"}"

and string_of_sig lab_vtyp_l = 
  List.fold_left 
   (fun acc (lab, ty) -> acc ^ "; " ^ lab ^ ":" ^ string_of_vtyp ty) String.empty lab_vtyp_l

let extract_return_type = function (TComp (vty, _)) -> vty

(* We provide a way to generate fresh type variables,
   that are used in the type checker *)
let count_typevar = ref 0

let fresh_typevar () =
  let a = !count_typevar in
  count_typevar := !count_typevar + 1;
  TVar ("'a" ^ string_of_int a)

(* We provide a way to generate fresh type name,
   that are used in nup generator *)
let count_typename = ref 0

let fresh_typename () =
  let a = !count_typename in
  count_typename := !count_typename + 1;
  "a" ^ string_of_int a


(* Typing contexts *)

(*module VarCtx = Map.Make(
  struct 
    type t = typevar
    let compare = String.compare
end)

(*type var_ctx = vtyp VarCtx.t *)
let empty_var_ctx = VarCtx.empty


module ConsCtx = Map.Make(String)

type cons_ctx = cons_sig ConsCtx.t
let empty_cons_ctx = ConsCtx.empty
let cons_ctx_of_list = fun l ->
  List.fold_left
    (fun tsubst (tvar, a) -> 
      ConsCtx.add tvar a tsubst) 
    ConsCtx.empty l


(* module NameCtx = Map.Make(
  struct 
    type t = Names.name
    let compare n n' = 
     String.compare (Names.string_of_name n) (Names.string_of_name n')
end) *)

module TypeSubst = Map.Make(
  struct 
    type t = typevar
    let compare = String.compare
end)

type 'a typesubst = 'a TypeSubst.t

let typesubst_empty = TypeSubst.empty
let typesubst_map = TypeSubst.map
let typesubst_filter_map = fun f_opt -> TypeSubst.filter_map (fun _ a -> f_opt a)
let typesubst_list_to_map = fun tsubst_l -> 
  List.fold_left
    (fun tsubst (tvar, a) -> 
      TypeSubst.add tvar a tsubst) 
    TypeSubst.empty
    tsubst_l
*)

type var_ctx = (id, vtyp) Util.Pmap.pmap
type name_ctx = (Names.name, vtyp) Util.Pmap.pmap
type cons_ctx = (label, cons_sig) Util.Pmap.pmap
let cons_ctx_of_list = fun l ->
  List.fold_left
    (fun tsubst (tvar, a) -> 
      Util.Pmap.add (tvar, a) tsubst) 
    Util.Pmap.empty l

type type_subst = (typevar, vtyp) Util.Pmap.pmap
let typesubst_list_to_map = fun tsubst_l -> 
  List.fold_left
    (fun tsubst (tvar, a) -> 
      Util.Pmap.add (tvar, a) tsubst) 
    Util.Pmap.empty
    tsubst_l

type type_env = (typevar, vtyp) Util.Pmap.pmap

type type_ctx = { 
  var_ctx: var_ctx;
  cons_ctx: cons_ctx ; 
  name_ctx: name_ctx;
  type_subst: type_subst}

let get_name_ctx typectx = typectx.name_ctx

let rec subst_vtype tvar sty ty =
  match ty with
  | TUnit | TInt | TBool | TId _ | TName _ -> ty
  | TArrow (vty, cty) ->
      TArrow (subst_vtype tvar sty vty, subst_ctype tvar sty cty)
  | TProd (ty1, ty2) -> TProd (subst_vtype tvar sty ty1, subst_vtype tvar sty ty2)
  | TVar tvar' when tvar = tvar' -> sty
  | TVar _ -> ty
  | TForall _ -> ty  
  | _ -> failwith "Error: undefined type, please report."

and subst_ctype tvar sty cty = 
  match cty with 
  | TComp(vty, effty) -> 
    TComp((subst_vtype tvar sty vty), effty)

(* The following function perform nested substitution of subst on ty *)
let lsubst_type type_subst ty =
  Util.Pmap.fold
    (fun ty (tvar, sty) -> subst_vtype tvar sty ty)
    ty type_subst

let lsubst_vctx type_subst =
  Util.Pmap.map_im (fun vty -> lsubst_type type_subst vty)

let lsubst_nctx type_subst =
  Util.Pmap.map_im (fun vty -> lsubst_type type_subst vty)

(* let lsubst_vctx lsubst = Util.Pmap.map_im (fun ty -> lsubst_type lsubst ty)
*)
let subst_vctx tvar sty =
  Util.Pmap.map_im (fun ty -> subst_vtype tvar sty ty)

let subst_nctx tvar sty =
  Util.Pmap.map_im (fun ty -> subst_vtype tvar sty ty)

let extend_type_subst type_ctx tvar ty =
  let var_ctx = subst_vctx tvar ty type_ctx.var_ctx in
  let name_ctx = subst_nctx tvar ty type_ctx.name_ctx in
  let type_subst = Util.Pmap.add (tvar, ty) type_ctx.type_subst in
  { type_ctx with var_ctx; name_ctx; type_subst}

let extend_var_ctx type_ctx var ty =
  { type_ctx with var_ctx= Util.Pmap.add (var, ty) type_ctx.var_ctx }


let update_type_subst type_ctx type_subst =
  let var_ctx = lsubst_vctx type_subst type_ctx.var_ctx in
  let name_ctx = lsubst_nctx type_subst type_ctx.name_ctx in
  { type_ctx with var_ctx; name_ctx; type_subst }

(* The following function
   taking an initial type substitution
   and two types as input
   and tries to find a type susbtitution extending the initial one that unifies the two types, otherwise it returns None *)
let rec unify_type tsubst = function
  | (TUnit, TUnit) -> Some (TUnit, tsubst)
  | (TInt, TInt) -> Some (TInt, tsubst)
  | (TBool, TBool) -> Some (TBool, tsubst)
  | (TArrow (vty1, cty1), TArrow (vty2, cty2)) -> begin
      match unify_type tsubst (vty1, vty2) with
      | None ->
          Util.Debug.print_debug
            ("Cannot unify " ^ string_of_vtyp vty1 ^ " and "
           ^ string_of_vtyp vty1);
          None
      | Some (vty, tsubst') -> (
          match unify_ctype tsubst' (cty1, cty2) with
          | None ->
              Util.Debug.print_debug
                ("Cannot unify " ^ string_of_ctyp cty1 ^ " and "
               ^ string_of_ctyp cty2);
              None
          | Some (cty, tsubst'') -> Some (TArrow (vty, cty), tsubst''))
    end
  | (TProd (ty11, ty12), TProd (ty21, ty22)) -> begin
      match unify_type tsubst (ty11, ty21) with
      | None ->
          Util.Debug.print_debug
            ("Cannot unify " ^ string_of_vtyp ty11 ^ " and "
           ^ string_of_vtyp ty21);
          None
      | Some (ty1, lsubst') -> (
          match unify_type lsubst' (ty12, ty22) with
          | None ->
              Util.Debug.print_debug
                ("Cannot unify " ^ string_of_vtyp ty12 ^ " and "
               ^ string_of_vtyp ty22);
              None
          | Some (ty2, lsubst'') -> Some (TProd (ty1, ty2), lsubst''))
    end
  | ((TVar tvar1 as ty1), TVar tvar2) when tvar1 = tvar2 -> Some (ty1, tsubst)
  | ((TVar _ as ty1), TVar tvar2) ->
      Some (ty1, Util.Pmap.add (tvar2, ty1) tsubst)
  | (TVar tvar, ty) | (ty, TVar tvar) -> begin
      match Util.Pmap.lookup tvar tsubst with
      | None -> Some (ty, Util.Pmap.add (tvar, ty) tsubst)
      | Some ty' -> begin
          match unify_type tsubst (ty, ty') with
          | None ->
              Util.Debug.print_debug
                ("Cannot unify " ^ string_of_vtyp ty ^ " and "
               ^ string_of_vtyp ty');
              None
          | Some (ty'', lsubst'') ->
              Some (ty'', Util.Pmap.add (tvar, ty'') lsubst'')
        end
    end
  | (ty1, ty2) ->
      Util.Debug.print_debug
        ("Cannot unify " ^ string_of_vtyp ty1 ^ " and " ^ string_of_vtyp ty2);
      None

and unify_ctype tsubst = function 
  | (TComp (vty1, effty), TComp (vty2, _)) -> 
    match unify_type tsubst (vty1, vty2) with 
      | None -> Util.Debug.print_debug
        ("Cannot unify " ^ string_of_vtyp vty1 ^ " and " ^ string_of_vtyp vty2);
        None
      | Some (vty, tsubst') -> Some (TComp (vty, effty), tsubst')


(* The following function perform parallel substitution of subst on ty *)
let rec apply_vtype_subst ty subst =
  match ty with
  | TUnit | TInt | TBool | TName _ | TId _ -> ty
  | TArrow (vty, cty) ->
      TArrow (apply_vtype_subst vty subst, apply_ctype_subst cty subst)
  | TProd (ty1, ty2) ->
      TProd (apply_vtype_subst ty1 subst, apply_vtype_subst ty2 subst)
  | TVar tvar -> begin
      match Util.Pmap.lookup tvar subst with Some ty' -> ty' | None -> ty
    end
  | TVariant labelled_types -> TVariant
      (List.map
       (fun (lab, vty) -> (lab, apply_vtype_subst vty subst)) 
       labelled_types)  
  | TRecord labelled_types -> TRecord
      (List.map
       (fun (lab, vty) -> (lab, apply_vtype_subst vty subst)) 
       labelled_types)  
  | TForall _ ->
      failwith
        "Applying type substitution on universally quantified type is not \
         supported. Please report."
  | _ -> failwith "Error: undefined type, please report."

and apply_ctype_subst cty subst = 
  match cty with TComp (vty, effty) -> 
    TComp (apply_vtype_subst vty subst, effty)

(* Interaction *)

type negative_type = vtyp

let string_of_negative_type = string_of_vtyp

let get_negative_type ty =
  match ty with TArrow _ | TForall _ -> Some ty | _ -> None

let get_negative_ctype cty =
  match cty with TComp (vty, _) -> 
    match get_negative_type vty with 
      | Some vty -> Some (TComp (vty, effpure))
      | None -> None
let force_negative_type ty = ty

let rec apply_vtype_env ty type_env =
  match ty with
  | TUnit | TInt | TBool | TName _ | TVar _  -> ty
  | TArrow (ty1, ty2) ->
      TArrow (apply_vtype_env ty1 type_env, apply_ctype_env ty2 type_env)
  | TProd (ty1, ty2) ->
      TProd (apply_vtype_env ty1 type_env, apply_vtype_env ty2 type_env)
  | TForall (tvar_l, ty') -> TForall (tvar_l, apply_vtype_env ty' type_env)
  | TId id -> begin
      match Util.Pmap.lookup id type_env with Some ty' -> ty' | None -> ty
    end
  | _ -> failwith "Error: undefined type, please report."

and apply_ctype_env cty type_env = 
  match cty with TComp (vty, eff) -> TComp (apply_vtype_env vty type_env, eff)
