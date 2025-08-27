(* Typing contexts for variables, locations and constructors *)

type var_ctx = (Syntax.id, Types.typ) Util.Pmap.pmap
type loc_ctx = (Syntax.loc, Types.typ) Util.Pmap.pmap
type cons_ctx = (Syntax.constructor, Types.typ) Util.Pmap.pmap

let empty_var_ctx = Util.Pmap.empty
let empty_loc_ctx = Util.Pmap.empty
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

let pp_cons_ctx fmt cons_ctx =
  let pp_empty fmt () = Format.fprintf fmt "⋅" in
  let pp_pair fmt (c, ty) =
    Format.fprintf fmt "%a : %a" Syntax.pp_constructor c Types.pp_typ ty in
  Util.Pmap.pp_pmap ~pp_empty pp_pair fmt cons_ctx

let string_of_var_ctx = Format.asprintf "%a" pp_var_ctx
let string_of_loc_ctx = Format.asprintf "%a" pp_loc_ctx
let string_of_cons_ctx = Format.asprintf "%a" pp_cons_ctx

type type_ctx = {
  var_ctx: var_ctx;
  loc_ctx: loc_ctx;
  name_ctx: Namectx.Namectx.t;
  cons_ctx: cons_ctx;
  type_env: Types.type_env;
}

let get_var_ctx type_ctx = type_ctx.var_ctx
let get_name_ctx type_ctx = type_ctx.name_ctx
let get_loc_ctx type_ctx = type_ctx.loc_ctx
let get_type_env type_ctx = type_ctx.type_env

let extend_var_ctx type_ctx var ty =
  { type_ctx with var_ctx= Util.Pmap.modadd (var, ty) type_ctx.var_ctx }

let apply_type_subst_to_ctx tsubst =
  Util.Pmap.map_im (fun ty -> Types.apply_type_subst ty tsubst)

let apply_type_subst type_ctx tsubst =
  let var_ctx = apply_type_subst_to_ctx tsubst type_ctx.var_ctx in
  let loc_ctx = apply_type_subst_to_ctx tsubst type_ctx.loc_ctx in
  let name_ctx =
    Namectx.Namectx.map (fun ty -> Types.apply_type_subst ty tsubst) type_ctx.name_ctx
  in
  let cons_ctx = apply_type_subst_to_ctx tsubst type_ctx.cons_ctx in
  let type_env = type_ctx.type_env in
  { var_ctx; loc_ctx; name_ctx; cons_ctx; type_env }

let build_type_ctx _expr =
  {
    var_ctx= empty_var_ctx;
    loc_ctx= empty_loc_ctx;
    name_ctx = Namectx.Namectx.empty;
    cons_ctx= empty_cons_ctx;
    type_env= Types.empty_type_env;
  }
