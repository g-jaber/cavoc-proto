(* This file contain the simple modules that we consider for this language, with their signatures *)

type signature_decl =
  | PrivateTypeDecl of Types.id
  | PublicTypeDecl of (Types.id * Types.typeML)
  | PublicValDecl of (Syntax.id * Types.typeML)

val string_of_signature_decl : signature_decl -> string
val string_of_signature : signature_decl list -> string

type implem_decl =
  | TypeDecl of (Types.id * Types.typeML)
  | ValDecl of (Syntax.id * Syntax.exprML)

val string_of_implem_decl : implem_decl -> string
val string_of_prog : implem_decl list -> string
val extract_type_subst : implem_decl list -> Types.type_subst

type comp_env = (Syntax.id * Syntax.exprML) list

val get_typed_comp_env : implem_decl list -> comp_env * Type_ctx.type_ctx

val get_typed_int_env :
  Syntax.val_env ->
  signature_decl list ->
    (Syntax.name,Syntax.valML) Util.Pmap.pmap * Type_ctx.name_ctx
