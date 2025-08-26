(* This file contain the simple modules that we consider for this language, with their signatures *)

type signature_decl =
  | PrivateTypeDecl of Types.id
  | PublicTypeDecl of (Types.id * Types.typ)
  | PublicValDecl of (Syntax.id * Types.typ)
  | PublicExnDecl of (Syntax.constructor * Types.typ option)

val string_of_signature_decl : signature_decl -> string
val string_of_signature : signature_decl list -> string

type implem_decl =
  | TypeDecl of (Types.id * Types.typ)
  | ValDecl of (Syntax.id * Syntax.term)
  | ExnDecl of (Syntax.constructor * Types.typ option)

val string_of_implem_decl : implem_decl -> string
val string_of_prog : implem_decl list -> string
val extract_type_subst : implem_decl list -> Types.type_subst

type comp_env = (Syntax.id * Syntax.term) list

val get_typed_comp_env :
  implem_decl list -> signature_decl list -> comp_env * Namectx.Namectx.t * Type_ctx.cons_ctx

(* get_typed_val_env  takes a map from variables to values (i.e. evaluated computations) and a signature,
and return an interactive env, together with the typing contexts of these Proponent names.
The Proponent names are taken in correspondance to the variables that are declared in the given signature. *)
val get_typed_val_env :
  Syntax.val_env ->
  signature_decl list ->
  Ienv.IEnv.t * Namectx.Namectx.t
