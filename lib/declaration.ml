type decl_attribute =
  | Public
  | Private

let string_of_attribute = function
  | Public -> "Public"
  | Private -> "Private"


type decl =
  |  TypeDecl of (decl_attribute*Types.typevar*Types.typeML)
  |  TypeValDecl of (decl_attribute*Syntax.id*Types.typeML)
  |  ValDecl of (Syntax.id*Syntax.exprML)

let string_of_decl = function
  | TypeDecl (attr, tvar, ty) ->
    (string_of_attribute attr) ^ " type " ^ tvar ^ " = " ^ (Types.string_of_typeML ty)
  | TypeValDecl (attr, var, ty) ->
    (string_of_attribute attr) ^ " val " ^ var ^ " : " ^ (Types.string_of_typeML ty)
  | ValDecl (var, expr) ->
    " let " ^ var ^ " = " ^ (Syntax.string_of_exprML expr)

let string_of_program prog =
  String.concat "\n" ((List.map string_of_decl) prog)

let extract_type_subst prog = 
  let aux = function
  | TypeDecl (_, tvar, ty) -> Some (tvar,ty)
  | _ -> None
in Pmap.list_to_pmap (List.filter_map aux prog)