(* Typing contexts for variables, locations and names *)

type var_ctx = (Syntax.id, Types.typeML) Util.Pmap.pmap
type loc_ctx = (Syntax.loc, Types.typeML) Util.Pmap.pmap
type name_ctx = (Syntax.name, Types.typeML) Util.Pmap.pmap

let empty_name_ctx = Util.Pmap.empty
let empty_var_ctx = Util.Pmap.empty
let empty_loc_ctx = Util.Pmap.empty

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

type type_ctx = {
  var_ctx: var_ctx;
  loc_ctx: loc_ctx;
  name_ctx: name_ctx;
  exn_ctx: Syntax.constructor list;
  type_subst: Types.type_subst;
}

let get_var_ctx type_ctx = type_ctx.var_ctx
let get_name_ctx type_ctx = type_ctx.name_ctx
let get_loc_ctx type_ctx = type_ctx.loc_ctx
let get_type_subst type_ctx = type_ctx.type_subst

let subst_ctx tvar sty =
  Util.Pmap.map_im (fun ty -> Types.subst_type tvar sty ty)

(* TO BE REMOVED *)
let subst_vctx tvar sty =
  Util.Pmap.map_im (fun ty -> Types.subst_type tvar sty ty)

(* The following function perform nested substitution of subst on ty *)
let lsubst_type type_subst ty =
  Util.Pmap.fold (fun ty (tvar, sty) -> Types.subst_type tvar sty ty) ty type_subst

let lsubst_vctx lsubst = Util.Pmap.map_im (fun ty -> lsubst_type lsubst ty)

let lsubst_ctx type_subst = Util.Pmap.map_im (fun ty -> lsubst_type type_subst ty)

let extend_type_subst type_ctx tvar ty =
  let var_ctx = subst_ctx tvar ty type_ctx.var_ctx in
  let loc_ctx = subst_ctx tvar ty type_ctx.loc_ctx in
  let name_ctx = subst_ctx tvar ty type_ctx.name_ctx in
  let type_subst = Util.Pmap.modadd_pmap (tvar,ty) type_ctx.type_subst in
  {type_ctx with var_ctx; loc_ctx; name_ctx; type_subst}

let update_type_subst type_ctx type_subst =
  let var_ctx = lsubst_ctx type_subst type_ctx.var_ctx in
  let loc_ctx = lsubst_ctx type_subst type_ctx.loc_ctx in
  let name_ctx = lsubst_ctx type_subst type_ctx.name_ctx in
  {type_ctx with var_ctx; loc_ctx; name_ctx; type_subst}

let extend_var_ctx type_ctx var ty
  = {type_ctx with var_ctx = Util.Pmap.modadd_pmap (var, ty) type_ctx.var_ctx}