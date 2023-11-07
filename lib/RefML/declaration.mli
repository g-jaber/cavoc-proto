(* This file contain the simple modules that we consider for this language, with their signatures *)

type signature_decl =
  | PrivateTypeDecl of Types.typevar
  | PublicTypeDecl of (Types.typevar * Types.typeML)
  | PublicValDecl of (Syntax.id * Types.typeML)

val string_of_signature_decl : signature_decl -> string
val string_of_signature : signature_decl list -> string

type implem_decl =
  | TypeDecl of (Types.typevar * Types.typeML)
  | ValDecl of (Syntax.id * Syntax.exprML)

val string_of_implem_decl : implem_decl -> string
val string_of_prog : implem_decl list -> string
val extract_type_subst : implem_decl list -> Types.type_subst

type comp_env = (Syntax.id * Syntax.exprML) list

val get_comp_env : implem_decl list -> comp_env * Type_ctx.var_ctx

val get_ienv : signature_decl list -> Focusing.interactive_env * Type_ctx.name_ctx