(* This file contain the simple modules that we consider for this language, with their signatures *)

type signature_decl =
  | PrivateTypeDecl of Types.id
  | PublicTypeDecl of (Types.id * Types.typ)
  | PublicValDecl of (Syntax.id * Types.typ)
  | PublicExnDecl of (Syntax.constructor * Types.typ)

val string_of_signature_decl : signature_decl -> string
val string_of_signature : signature_decl list -> string

type implem_decl =
  | TypeDecl of (Types.id * Types.typ)
  | ValDecl of (Syntax.id * Syntax.term)
  | ExnDecl of (Syntax.constructor * Types.typ)

val string_of_implem_decl : implem_decl -> string
val string_of_prog : implem_decl list -> string
val extract_type_subst : implem_decl list -> Types.type_subst

type comp_env = (Syntax.id * Syntax.term) list

val get_typed_comp_env : implem_decl list -> comp_env * Type_ctx.name_ctx * Type_ctx.cons_ctx

val get_typed_val_env :
  Syntax.val_env ->
  signature_decl list ->
    (Names.name ,Syntax.value) Util.Pmap.pmap * Type_ctx.name_ctx
