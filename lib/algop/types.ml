type typevar = string
type rowvar = string
type id = string
type label = string
type opsymbol = string


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

module VarCtx = Map.Make(
  struct 
    type t = typevar
    let compare = String.compare
end)

module ConsCtx = Map.Make(String)

module NameCtx = Map.Make(
  struct 
    type t = Names.name
    let compare n n' = 
     String.compare (Names.string_of_name n) (Names.string_of_name n')
end)

module TypeSubst = Map.Make(
  struct 
    type t = typevar
    let compare = String.compare
end)

type type_ctx = { 
  var_ctx: vtyp VarCtx.t; 
  cons_ctx:cons_sig ConsCtx.t; 
  name_ctx: vtyp NameCtx.t;
  type_subst: vtyp TypeSubst.t}

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
  TypeSubst.fold
    (fun tvar sty ty -> subst_vtype tvar sty ty)
    type_subst ty

let lsubst_vctx type_subst =
  VarCtx.map (fun vty -> lsubst_type type_subst vty)

let lsubst_nctx type_subst =
  NameCtx.map (fun vty -> lsubst_type type_subst vty)

(* let lsubst_vctx lsubst = Util.Pmap.map_im (fun ty -> lsubst_type lsubst ty)

*)
let subst_vctx tvar sty =
  VarCtx.map (fun ty -> subst_vtype tvar sty ty)

let subst_nctx tvar sty =
  NameCtx.map (fun ty -> subst_vtype tvar sty ty)

let extend_type_subst type_ctx tvar ty =
  let var_ctx = subst_vctx tvar ty type_ctx.var_ctx in
  let name_ctx = subst_nctx tvar ty type_ctx.name_ctx in
  let type_subst = TypeSubst.add tvar ty type_ctx.type_subst in
  { type_ctx with var_ctx; name_ctx; type_subst}

let extend_var_ctx type_ctx var ty =
  { type_ctx with var_ctx= VarCtx.add var ty type_ctx.var_ctx }


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
      Some (ty1, TypeSubst.add tvar2 ty1 tsubst)
  | (TVar tvar, ty) | (ty, TVar tvar) -> begin
      match TypeSubst.find_opt tvar tsubst with
      | None -> Some (ty, TypeSubst.add tvar ty tsubst)
      | Some ty' -> begin
          match unify_type tsubst (ty, ty') with
          | None ->
              Util.Debug.print_debug
                ("Cannot unify " ^ string_of_vtyp ty ^ " and "
               ^ string_of_vtyp ty');
              None
          | Some (ty'', lsubst'') ->
              Some (ty'', TypeSubst.add tvar ty'' lsubst'')
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
      match TypeSubst.find_opt tvar subst with Some ty' -> ty' | None -> ty
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


(* Typing contexts for variables, locations and names *)


(*
let rec get_new_tvars tvar_set = function
  | TUnit | TInt | TBool | TExn | TId _ | TName _ | TUndef -> tvar_set
  | TArrow (ty1, ty2) | TProd (ty1, ty2) | TSum (ty1, ty2) ->
      let tvar_set' = get_new_tvars tvar_set ty1 in
      get_new_tvars tvar_set' ty2
  | TRef ty -> get_new_tvars tvar_set ty
  | TVar typevar -> TVarSet.add typevar tvar_set
  | TForall (tvars, ty) ->
      let tvar_set' = List.fold_left (Fun.flip TVarSet.remove) tvar_set tvars in
      get_new_tvars tvar_set' ty

let get_tvars ty = TVarSet.elements @@ get_new_tvars TVarSet.empty ty

(* Type substitutions are maps from type variables to types *)

type type_subst = (typevar, typ) Util.Pmap.pmap
type type_env = (id, typ) Util.Pmap.pmap

let rec apply_type_env ty type_env =
  match ty with
  | TUnit | TInt | TBool | TRef _ | TName _ | TVar _ | TExn -> ty
  | TArrow (ty1, ty2) ->
      TArrow (apply_type_env ty1 type_env, apply_type_env ty2 type_env)
  | TProd (ty1, ty2) ->
      TProd (apply_type_env ty1 type_env, apply_type_env ty2 type_env)
  | TSum (ty1, ty2) ->
      TSum (apply_type_env ty1 type_env, apply_type_env ty2 type_env)
  | TId id -> begin
      match Util.Pmap.lookup id type_env with Some ty' -> ty' | None -> ty
    end
  | TForall (tvar_l, ty') -> TForall (tvar_l, apply_type_env ty' type_env)
  | TUndef -> failwith "Error: undefined type, please report."

let rec subst_type tvar sty ty =
  match ty with
  | TUnit | TInt | TBool | TRef _ | TExn -> ty
  | TArrow (ty1, ty2) ->
      TArrow (subst_type tvar sty ty1, subst_type tvar sty ty2)
  | TProd (ty1, ty2) -> TProd (subst_type tvar sty ty1, subst_type tvar sty ty2)
  | TSum (ty1, ty2) -> TSum (subst_type tvar sty ty1, subst_type tvar sty ty2)
  | TVar tvar' when tvar = tvar' -> sty
  | TVar _ -> ty
  | TId _ | TName _ -> ty
  | TForall (tvars, ty') when List.mem tvar tvars ->
      TForall (tvars, subst_type tvar sty ty')
  | TForall _ -> ty
  | TUndef -> failwith "Error: undefined type, please report."


let generalize_type ty =
  let tvar_l = get_tvars ty in
  TForall (tvar_l, ty)

type negative_type = typ

let string_of_negative_type = string_of_typ

let get_negative_type ty =
  match ty with TArrow _ | TForall _ -> Some ty | _ -> None
let force_negative_type ty = ty
*)