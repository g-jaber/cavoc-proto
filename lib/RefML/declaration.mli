type signature_decl =
  |  PrivateTypeDecl of Types.typevar
  |  PublicTypeDecl of (Types.typevar*Types.typeML)
  |  PublicValDecl of (Syntax.id*Types.typeML)

val string_of_signature_decl : signature_decl -> string

val string_of_signature : signature_decl list -> string

type implem_decl =
  |  TypeDecl of (Types.typevar*Types.typeML)
  |  ValDecl of (Syntax.id*Syntax.exprML)

val string_of_implem_decl : implem_decl -> string

val string_of_prog : implem_decl list -> string

val extract_type_subst : implem_decl list -> Types.type_subst

val get_ienv :
  implem_decl list -> 
    (Focusing.interactive_env*Syntax.name_ctx)