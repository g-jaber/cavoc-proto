type typevar = string [@@deriving to_yojson]
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

let pp_tid fmt id = Format.fprintf fmt "%s" id
let pp_tname fmt n = Format.fprintf fmt "%s" n
let pp_tvar fmt typevar = Format.fprintf fmt "%s" typevar

let pp_tvar_l fmt tvar_l =
  let pp_sep fmt () = Format.fprintf fmt " " in
  Format.pp_print_list ~pp_sep pp_tvar fmt tvar_l

let rec pp_typ fmt = function
  | TUnit -> Format.fprintf fmt "unit"
  | TInt -> Format.fprintf fmt "int"
  | TBool -> Format.fprintf fmt "bool"
  | TArrow (ty1, ty2) -> Format.fprintf fmt "%a → %a" pp_par_typ ty1 pp_typ ty2
  | TProd (ty1, ty2) ->
      Format.fprintf fmt "%a × %a" pp_par_typ ty1 pp_par_typ ty2
  | TSum (ty1, ty2) ->
      Format.fprintf fmt "%a + %a" pp_par_typ ty1 pp_par_typ ty2
  | TRef ty -> Format.fprintf fmt "ref %a" pp_typ ty
  | TExn -> Format.fprintf fmt "exn"
  | TVar typevar -> pp_tvar fmt typevar
  | TForall ([], ty) -> Format.fprintf fmt "%a" pp_typ ty
  | TForall (tvar_l, ty) ->
      Format.fprintf fmt "∀%a . %a" pp_tvar_l tvar_l pp_typ ty
  | TId id -> pp_tid fmt id
  | TName n -> pp_tname fmt n
  | TUndef -> Format.fprintf fmt "undef"

and pp_par_typ fmt = function
  | TArrow (ty1, ty2) ->
      Format.fprintf fmt "(%a → %a)" pp_par_typ ty1 pp_typ ty2
  | TProd (ty1, ty2) ->
      Format.fprintf fmt "(%a × %a)" pp_par_typ ty1 pp_par_typ ty2
  | TSum (ty1, ty2) ->
      Format.fprintf fmt "(%a + %a)" pp_par_typ ty1 pp_par_typ ty2
  | TRef ty -> Format.fprintf fmt "(ref %a)" pp_typ ty
  | TForall ([], ty) -> Format.fprintf fmt "%a" pp_typ ty
  | TForall (tvar_l, ty) ->
      Format.fprintf fmt "(∀%a . %a)" pp_tvar_l tvar_l pp_typ ty
  | typ -> pp_typ fmt typ

let string_of_typ = Format.asprintf "%a" pp_typ
let typ_to_yojson ty = `String (string_of_typ ty)

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

let rec get_new_free_tvars tvar_set = function
  | TUnit | TInt | TBool | TExn | TId _ | TName _ | TUndef -> tvar_set
  | TArrow (ty1, ty2) | TProd (ty1, ty2) | TSum (ty1, ty2) ->
      let tvar_set' = get_new_free_tvars tvar_set ty1 in
      get_new_free_tvars tvar_set' ty2
  | TRef ty -> get_new_free_tvars tvar_set ty
  | TVar typevar -> TVarSet.add typevar tvar_set
  | TForall (tvars, ty) ->
      let tvar_set' = List.fold_left (Fun.flip TVarSet.remove) tvar_set tvars in
      get_new_free_tvars tvar_set' ty

let get_free_tvars ty = TVarSet.elements @@ get_new_free_tvars TVarSet.empty ty

let generalize_type ty =
  let tvar_l = get_free_tvars ty in
  match tvar_l with [] -> ty | _ -> TForall (tvar_l, ty)

(* Type substitutions are maps from type variables to types *)

type type_subst = (typevar, typ) Util.Pmap.pmap

let empty_type_subst = Util.Pmap.empty

let string_of_type_subst =
  Util.Pmap.string_of_pmap "[]" "=" Fun.id string_of_typ

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
      @@ "Error applying type substitution on universally quantified type "
      ^ string_of_typ ty
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

let subst_in_tsubst tsubst tvar ty =
  Util.Pmap.map (fun (tvar', ty') -> (tvar', subst_type tvar ty ty')) tsubst

let compose_type_subst tsubst1 tsubst2 =
  let tsubst2' =
    Util.Pmap.map
      (fun (tvar, ty) -> (tvar, apply_type_subst ty tsubst1))
      tsubst2 in
  Util.Pmap.concat tsubst1 tsubst2'

type type_env = (id, typ) Util.Pmap.pmap

let empty_type_env = Util.Pmap.empty

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

let mgu_type tenv (ty1, ty2) =
  let rec mgu_type_aux ty1 ty2 tsubst =
    match (ty1, ty2) with
    | (TUnit, TUnit) | (TInt, TInt) | (TBool, TBool) | (TExn, TExn) ->
        Some tsubst
    | (TRef ty1, TRef ty2) -> mgu_type_aux ty1 ty2 tsubst
    | (TArrow (ty11, ty12), TArrow (ty21, ty22))
    | (TProd (ty11, ty12), TProd (ty21, ty22)) -> begin
        match mgu_type_aux ty11 ty21 tsubst with
        | None -> None
        | Some tsubst' ->
            let ty12' = apply_type_subst ty12 tsubst' in
            let ty22' = apply_type_subst ty22 tsubst' in
            mgu_type_aux ty12' ty22' tsubst'
      end
    | (TVar tvar1, TVar tvar2) when tvar1 = tvar2 -> Some tsubst
    | (TVar tvar, ty) | (ty, TVar tvar) ->
        Util.Debug.print_debug @@ "New constraint: " ^ tvar ^ " = "
        ^ string_of_typ ty;
        let tsubst' = subst_in_tsubst tsubst tvar ty in
        Some (Util.Pmap.add (tvar, ty) tsubst')
        (* We should do some occur_check *)
    | (TId id, ty) | (ty, TId id) -> begin
        match Util.Pmap.lookup id tenv with
        | Some ty' -> mgu_type_aux ty ty' tsubst
        | None -> None
      end
    | (TForall _, TForall _) ->
        failwith
          "Computing the MGU of forall types is not yet supported. Please \
           report."
    | (ty1, ty2) ->
        Util.Debug.print_debug
          ("Cannot unify " ^ string_of_typ ty1 ^ " and " ^ string_of_typ ty2);
        None in
  mgu_type_aux ty1 ty2 Util.Pmap.empty

let refresh_forall = function
  | TForall (tvar_l, ty) ->
      let tsubst =
        Util.Pmap.list_to_pmap
          (List.map
             (fun tvar ->
               let tvar' = fresh_typevar () in
               (tvar, tvar'))
             tvar_l) in
      let ty' = apply_type_subst ty tsubst in
      ty'
  | ty -> ty

type negative_type = typ

let string_of_negative_type = string_of_typ
let pp_negative_type = pp_typ
let negative_type_to_yojson ty = `String (string_of_negative_type ty)


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
      failwith @@ "Error retrieving an output type: the type "
      ^ string_of_typ ty ^ " is not a negative type. Please report."
