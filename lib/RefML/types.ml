type typevar = string
type id = string

(* Types *)
type typ =
  | TUnit
  | TInt
  | TBool
  | TArrow of typ * typ
  | TProd of typ * typ
  | TSum of typ * typ
  | TRef of typ
  | TExn
  | TVar of typevar
  | TForall of typevar list * typ
  | TId of id
  | TName of id
  | TUndef

let rec string_par_of_typ = function
  | TUnit -> "Unit"
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow (ty1, ty2) ->
      "(" ^ string_par_of_typ ty1 ^ "->" ^ string_of_typ ty2 ^ ")"
  | TProd (ty1, ty2) ->
      "(" ^ string_par_of_typ ty1 ^ "*" ^ string_par_of_typ ty2 ^ ")"
  | TSum (ty1, ty2) ->
      "(" ^ string_par_of_typ ty1 ^ "+" ^ string_par_of_typ ty2 ^ ")"
  | TRef ty -> "(ref " ^ string_of_typ ty ^ ")"
  | TExn -> "exn"
  | TVar typevar -> typevar
  | TForall (tvars, ty) ->
      let tvars_string = String.concat " " tvars in
      "(∀" ^ tvars_string ^ "." ^ string_par_of_typ ty ^ ")"
  | TId id -> id
  | TName id -> id
  | TUndef -> "undef"

and string_of_typ = function
  | TUnit -> "Unit"
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow (ty1, ty2) -> string_par_of_typ ty1 ^ "->" ^ string_of_typ ty2
  | TProd (ty1, ty2) ->
      string_par_of_typ ty1 ^ "*" ^ string_par_of_typ ty2
  | TSum (ty1, ty2) -> string_par_of_typ ty1 ^ "+" ^ string_par_of_typ ty2
  | TRef ty -> "ref " ^ string_of_typ ty
  | TExn -> "exn"
  | TVar typevar -> typevar
  | TForall (tvars, ty) ->
      let tvars_string = String.concat " " tvars in
      "∀" ^ tvars_string ^ "." ^ string_par_of_typ ty
  | TId id -> id
  | TName id -> id
  | TUndef -> "undef"

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

module TVarSet = Set.Make (struct
  type t = typevar

  let compare = String.compare
end)

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

(* The following function perform parallel substitution of subst on ty *)
let rec apply_type_subst ty subst =
  match ty with
  | TUnit | TInt | TBool | TName _ | TRef _ | TExn -> ty
  | TArrow (ty1, ty2) ->
      TArrow (apply_type_subst ty1 subst, apply_type_subst ty2 subst)
  | TProd (ty1, ty2) ->
      TProd (apply_type_subst ty1 subst, apply_type_subst ty2 subst)
  | TSum (ty1, ty2) ->
      TSum (apply_type_subst ty1 subst, apply_type_subst ty2 subst)
  | TVar tvar -> begin
      match Util.Pmap.lookup tvar subst with Some ty' -> ty' | None -> ty
    end
  | TId _ -> ty
  | TForall _ ->
      failwith
        "Applying type substitution on universally quantified type is not \
         supported. Please report."
  | TUndef -> failwith "Error: undefined type, please report."

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

(* The following function
   taking an initial type substitution
   and two types as input
   and tries to find a type susbtitution extending the initial one that unifies the two types, otherwise it returns None *)
let rec unify_type tsubst = function
  | (TUnit, TUnit) -> Some (TUnit, tsubst)
  | (TInt, TInt) -> Some (TInt, tsubst)
  | (TBool, TBool) -> Some (TBool, tsubst)
  | (TRef ty1, TRef ty2) -> begin
      match unify_type tsubst (ty1, ty2) with
      | None -> None
      | Some (ty, tsubst') -> Some (TRef ty, tsubst')
    end
  | (TArrow (ty11, ty12), TArrow (ty21, ty22)) -> begin
      match unify_type tsubst (ty11, ty21) with
      | None ->
          Util.Debug.print_debug
            ("Cannot unify " ^ string_of_typ ty11 ^ " and "
           ^ string_of_typ ty21);
          None
      | Some (ty1, tsubst') -> (
          match unify_type tsubst' (ty12, ty22) with
          | None ->
              Util.Debug.print_debug
                ("Cannot unify " ^ string_of_typ ty12 ^ " and "
               ^ string_of_typ ty22);
              None
          | Some (ty2, tsubst'') -> Some (TArrow (ty1, ty2), tsubst''))
    end
  | (TProd (ty11, ty12), TProd (ty21, ty22)) -> begin
      match unify_type tsubst (ty11, ty21) with
      | None ->
          Util.Debug.print_debug
            ("Cannot unify " ^ string_of_typ ty11 ^ " and "
           ^ string_of_typ ty21);
          None
      | Some (ty1, lsubst') -> (
          match unify_type lsubst' (ty12, ty22) with
          | None ->
              Util.Debug.print_debug
                ("Cannot unify " ^ string_of_typ ty12 ^ " and "
               ^ string_of_typ ty22);
              None
          | Some (ty2, lsubst'') -> Some (TProd (ty1, ty2), lsubst''))
    end
  | (TExn, TExn) -> Some (TExn, tsubst)
  | ((TVar tvar1 as ty1), TVar tvar2) when tvar1 = tvar2 -> Some (ty1, tsubst)
  | ((TVar _ as ty1), TVar tvar2) ->
      Some (ty1, Util.Pmap.modadd (tvar2, ty1) tsubst)
  | (TVar tvar, ty) | (ty, TVar tvar) -> begin
      match Util.Pmap.lookup tvar tsubst with
      | None -> Some (ty, Util.Pmap.modadd (tvar, ty) tsubst)
      | Some ty' -> begin
          match unify_type tsubst (ty, ty') with
          | None ->
              Util.Debug.print_debug
                ("Cannot unify " ^ string_of_typ ty ^ " and "
               ^ string_of_typ ty');
              None
          | Some (ty'', lsubst'') ->
              Some (ty'', Util.Pmap.modadd (tvar, ty'') lsubst'')
        end
    end
  | ((TId id1 as ty), TId id2) | ((TName id1 as ty), TName id2) ->
      if id1 = id2 then Some (ty, tsubst) else None
  | (ty1, ty2) ->
      Util.Debug.print_debug
        ("Cannot unify " ^ string_of_typ ty1 ^ " and " ^ string_of_typ ty2);
      None

let generalize_type ty =
  let tvar_l = get_tvars ty in
  TForall (tvar_l, ty)

type negative_type = typ

let string_of_negative_type = string_of_typ

let get_negative_type ty =
  match ty with TArrow _ | TForall _ -> Some ty | _ -> None
let force_negative_type ty = ty

let get_input_type = function
| TArrow (ty1, _) -> ([], ty1)
| TForall (tvar_l, TArrow (ty1, _)) -> (tvar_l, ty1)
| ty ->
    failwith @@ "Error retrieving an input type: the type " ^ string_of_typ ty
    ^ " is not a negative type. Please report."

let get_output_type = function
| TArrow (_, ty2) -> ty2
| TForall (_, TArrow (_, ty2)) -> ty2
| ty ->
    failwith @@ "Error retrieving an output type: the type " ^ string_of_typ ty
    ^ " is not a negative type. Please report."