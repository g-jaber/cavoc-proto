(* Typing contexts for variables, locations and names *)

type var_ctx = (Syntax.id, Types.typ) Util.Pmap.pmap
type loc_ctx = (Syntax.loc, Types.typ) Util.Pmap.pmap
type name_ctx = (Names.name, Types.typ) Util.Pmap.pmap
type cons_ctx = (Syntax.constructor, Types.typ) Util.Pmap.pmap
(*beware that names can be typed by non-negative types here !*)

let empty_var_ctx = Util.Pmap.empty
let empty_loc_ctx = Util.Pmap.empty
let empty_name_ctx = Util.Pmap.empty
let empty_cons_ctx = Util.Pmap.empty

let pp_var_ctx fmt var_ctx =
  let pp_empty fmt () = Format.fprintf fmt "⋅" in
  let pp_pair fmt (x, ty) =
    Format.fprintf fmt "%a : %a" Syntax.pp_id x Types.pp_typ ty in
  Util.Pmap.pp_pmap ~pp_empty pp_pair fmt var_ctx

let pp_loc_ctx fmt loc_ctx =
  let pp_empty fmt () = Format.fprintf fmt "⋅" in
  let pp_pair fmt (l, ty) =
    Format.fprintf fmt "%a : %a" Syntax.pp_loc l Types.pp_typ ty in
  Util.Pmap.pp_pmap ~pp_empty pp_pair fmt loc_ctx

let pp_name_ctx fmt name_ctx =
  let pp_empty fmt () = Format.fprintf fmt "⋅" in
  let pp_pair fmt (n, ty) =
    Format.fprintf fmt "%a : %a" Names.pp_name n Types.pp_typ ty in
  Util.Pmap.pp_pmap ~pp_empty pp_pair fmt name_ctx

let pp_cons_ctx fmt cons_ctx =
  let pp_empty fmt () = Format.fprintf fmt "⋅" in
  let pp_pair fmt (c, ty) =
    Format.fprintf fmt "%a : %a" Syntax.pp_constructor c Types.pp_typ ty in
  Util.Pmap.pp_pmap ~pp_empty pp_pair fmt cons_ctx

let string_of_var_ctx = Format.asprintf "%a" pp_var_ctx

let string_of_loc_ctx = Format.asprintf "%a" pp_loc_ctx

let string_of_name_ctx = Format.asprintf "%a" pp_name_ctx

let string_of_cons_ctx =  Format.asprintf "%a" pp_cons_ctx

type type_ctx = {
  var_ctx: var_ctx;
  loc_ctx: loc_ctx;
  name_ctx: name_ctx;
  cons_ctx: cons_ctx;
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
  Util.Pmap.fold
    (fun ty (tvar, sty) -> Types.subst_type tvar sty ty)
    ty type_subst

let lsubst_vctx lsubst = Util.Pmap.map_im (fun ty -> lsubst_type lsubst ty)

let lsubst_ctx type_subst =
  Util.Pmap.map_im (fun ty -> lsubst_type type_subst ty)

let extend_type_subst type_ctx tvar ty =
  let var_ctx = subst_ctx tvar ty type_ctx.var_ctx in
  let loc_ctx = subst_ctx tvar ty type_ctx.loc_ctx in
  let name_ctx = subst_ctx tvar ty type_ctx.name_ctx in
  let type_subst = Util.Pmap.modadd (tvar, ty) type_ctx.type_subst in
  { type_ctx with var_ctx; loc_ctx; name_ctx; type_subst }

let update_type_subst type_ctx type_subst =
  let var_ctx = lsubst_ctx type_subst type_ctx.var_ctx in
  let loc_ctx = lsubst_ctx type_subst type_ctx.loc_ctx in
  let name_ctx = lsubst_ctx type_subst type_ctx.name_ctx in
  { type_ctx with var_ctx; loc_ctx; name_ctx; type_subst }

let extend_var_ctx type_ctx var ty =
  { type_ctx with var_ctx= Util.Pmap.modadd (var, ty) type_ctx.var_ctx }
