

type signature_decl =
  |  PrivateTypeDecl of Types.typevar
  |  PublicTypeDecl of (Types.typevar*Types.typeML)
  |  PublicValDecl of (Syntax.id*Types.typeML)

let string_of_signature_decl = function
  | PrivateTypeDecl (tvar) ->
    "type " ^ tvar
  | PublicTypeDecl (tvar, ty) ->
    "type " ^ tvar ^ " = " ^ (Types.string_of_typeML ty)
  | PublicValDecl (var, ty) ->
    " val " ^ var ^ " : " ^ (Types.string_of_typeML ty)

let string_of_signature signature =
  String.concat "\n" ((List.map string_of_signature_decl) signature)

let extract_type_subst signature = 
  let rec aux = function
  | [] -> []
  | PublicTypeDecl (tvar, ty)::l -> (tvar,ty)::(aux l)
  | _::l -> aux l
in Pmap.list_to_pmap (aux signature)


type implem_decl =
  |  TypeDecl of (Types.typevar*Types.typeML)
  |  ValDecl of (Syntax.id*Syntax.exprML)

let string_of_implem_decl = function
  | TypeDecl (tvar, ty) ->
    "type " ^ tvar ^ " = " ^ (Types.string_of_typeML ty)
  | ValDecl (var, exprML) ->
    " let " ^ var ^ " = " ^ (Syntax.string_of_exprML exprML)

let string_of_prog prog =
  String.concat "\n" ((List.map string_of_implem_decl) prog)