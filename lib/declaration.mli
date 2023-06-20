
type decl_attribute =
  | Public
  | Private

  type decl =
  |  TypeDecl of (decl_attribute*Syntax.id*Types.typeML)
  |  TypeValDecl of (decl_attribute*Syntax.id*Types.typeML)
  |  ValDecl of (Syntax.id*Syntax.exprML)

val string_of_decl : decl -> string

val string_of_program : decl list -> string

val extract_type_subst : decl list -> Types.type_subst