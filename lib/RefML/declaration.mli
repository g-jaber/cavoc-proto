(* This file handles the various declarations found in modules, programmes and signatures. *)

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

(* get_imported_name_env works on signature of *imported* declarations *)
val get_imported_name_env :
  signature_decl list ->
  Syntax.val_env * Type_ctx.var_ctx * Namectx.Namectx.t

val get_typed_comp_env :
  ?import_var_ctx:Type_ctx.var_ctx ->
  ?import_name_ctx:Namectx.Namectx.t ->
  implem_decl list -> signature_decl list -> comp_env * Namectx.Namectx.t * Type_ctx.cons_ctx

(* The interactive environment made of one Proponent name per declaration of the
   signature, bound to the value the given environment gives its identifier. *)
val get_typed_val_env :
  ?namectxO:Namectx.Namectx.t ->
  Syntax.val_env ->
  signature_decl list ->
  Ienv.IEnv.t * Namectx.Namectx.t
