type rowvar = string
type id = rowvar
type label = id
type opsymbol = label
type typevar = opsymbol
type vtyp =
    TUnit
  | TInt
  | TBool
  | TVar of typevar
  | TArrow of vtyp * ctyp
  | TRecord of (typevar * vtyp) list
  | TProd of vtyp * vtyp
  | TVariant of (typevar * vtyp) list
  | TForall of (typevar list * vtyp)
  | TId of typevar
  | TName of typevar
  | TUndef
and ctyp = TComp of vtyp * efftyp
and handler_typ = THandler of ctyp * ctyp
and rowpoly = Row of typevar | Closed
and efftyp = { effects : typevar list; poly : rowpoly; }
type cons_sig = Arity of vtyp * vtyp | Tcons of vtyp list * vtyp
val effpure : efftyp
val string_par_of_vtyp : vtyp -> typevar
val string_of_vtyp : vtyp -> typevar
val string_of_ctyp : ctyp -> string
val string_of_efftyp : efftyp -> string
val string_of_sig : (label * vtyp) list -> string
val extract_return_type : ctyp -> vtyp
val count_typevar : int ref
val fresh_typevar : unit -> vtyp
val count_typename : int ref
val fresh_typename : unit -> string
type var_ctx = (rowvar, vtyp) Util.Pmap.pmap
type name_ctx = (Names.name, vtyp) Util.Pmap.pmap
type cons_ctx = (rowvar, cons_sig) Util.Pmap.pmap
val cons_ctx_of_list : ('a * 'b) list -> ('a, 'b) Util.Pmap.pmap
type type_subst = (rowvar, vtyp) Util.Pmap.pmap
val typesubst_list_to_map : ('a * 'b) list -> ('a, 'b) Util.Pmap.pmap
type type_env = (rowvar, vtyp) Util.Pmap.pmap
type type_ctx = {
  var_ctx : var_ctx;
  cons_ctx : cons_ctx;
  name_ctx : name_ctx;
  type_subst : type_subst;
}
val get_name_ctx : type_ctx -> name_ctx
val subst_vtype : typevar -> vtyp -> vtyp -> vtyp
val subst_ctype : typevar -> vtyp -> ctyp -> ctyp
val lsubst_type : (typevar, vtyp) Util.Pmap.pmap -> vtyp -> vtyp
val lsubst_vctx :
  (typevar, vtyp) Util.Pmap.pmap ->
  ('a, vtyp) Util.Pmap.pmap -> ('a, vtyp) Util.Pmap.pmap
val lsubst_nctx :
  (typevar, vtyp) Util.Pmap.pmap ->
  ('a, vtyp) Util.Pmap.pmap -> ('a, vtyp) Util.Pmap.pmap
val subst_vctx :
  typevar -> vtyp -> ('a, vtyp) Util.Pmap.pmap -> ('a, vtyp) Util.Pmap.pmap
val subst_nctx :
  typevar -> vtyp -> ('a, vtyp) Util.Pmap.pmap -> ('a, vtyp) Util.Pmap.pmap
val extend_type_subst : type_ctx -> typevar -> vtyp -> type_ctx
val extend_var_ctx : type_ctx -> id -> vtyp -> type_ctx
val update_type_subst :
  type_ctx -> (typevar, vtyp) Util.Pmap.pmap -> type_ctx
val unify_type :
  (typevar, vtyp) Util.Pmap.pmap ->
  vtyp * vtyp -> (vtyp * (typevar, vtyp) Util.Pmap.pmap) option
val unify_ctype :
  (typevar, vtyp) Util.Pmap.pmap ->
  ctyp * ctyp -> (ctyp * (typevar, vtyp) Util.Pmap.pmap) option
val apply_vtype_subst : vtyp -> (typevar, vtyp) Util.Pmap.pmap -> vtyp
val apply_ctype_subst : ctyp -> (typevar, vtyp) Util.Pmap.pmap -> ctyp
type negative_type = vtyp
val string_of_negative_type : vtyp -> typevar
val get_negative_type : vtyp -> vtyp option
val get_negative_ctype : ctyp -> ctyp option
val force_negative_type : 'a -> 'a
val apply_vtype_env : vtyp -> (id, vtyp) Util.Pmap.pmap -> vtyp
val apply_ctype_env : ctyp -> (id, vtyp) Util.Pmap.pmap -> ctyp
