(* Typing contexts for variables, locations and names *)

type var_ctx = (Syntax.id, Types.typeML) Util.Pmap.pmap
type loc_ctx = (Syntax.loc, Types.typeML) Util.Pmap.pmap
type name_ctx = (Syntax.name, Types.typeML) Util.Pmap.pmap

let subst_vctx tvar sty =
  Util.Pmap.map_im (fun ty -> Types.subst_type tvar sty ty)

(* TODO: this function is slightly different with Types.apply_type_subst
   This should be clarified. *)
let lsubst_type lsubst ty =
  Util.Pmap.fold (fun ty (tvar, sty) -> Types.subst_type tvar sty ty) ty lsubst

let lsubst_vctx lsubst = Util.Pmap.map_im (fun ty -> lsubst_type lsubst ty)

let string_of_var_ctx =
  let aux = function
    | Types.TUndef -> "undef"
    | ty -> "::" ^ Types.string_of_typeML ty in
  Util.Pmap.string_of_pmap "ε" "" Syntax.string_of_id aux

let string_of_loc_ctx =
  let aux = function
    | Types.TUndef -> "undef"
    | ty -> "::" ^ Types.string_of_typeML ty in
  Util.Pmap.string_of_pmap "ε" "" Syntax.string_of_loc aux

let string_of_name_ctx =
  let aux = function
    | Types.TUndef -> "undef"
    | ty -> "::" ^ Types.string_of_typeML ty in
  Util.Pmap.string_of_pmap "ε" "" Syntax.string_of_name aux

let empty_name_ctx = Util.Pmap.empty
let empty_var_ctx = Util.Pmap.empty
let empty_loc_ctx = Util.Pmap.empty
