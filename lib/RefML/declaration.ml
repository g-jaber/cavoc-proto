type signature_decl =
  | PrivateTypeDecl of Types.typevar
  | PublicTypeDecl of (Types.typevar * Types.typeML)
  | PublicValDecl of (Syntax.id * Types.typeML)

let string_of_signature_decl = function
  | PrivateTypeDecl tvar -> "type " ^ tvar
  | PublicTypeDecl (tvar, ty) ->
      "type " ^ tvar ^ " = " ^ Types.string_of_typeML ty
  | PublicValDecl (var, ty) -> " val " ^ var ^ " : " ^ Types.string_of_typeML ty

let string_of_signature signature =
  String.concat "\n" ((List.map string_of_signature_decl) signature)

type implem_decl =
  | TypeDecl of (Types.typevar * Types.typeML)
  | ValDecl of (Syntax.id * Syntax.exprML)

let string_of_implem_decl = function
  | TypeDecl (tvar, ty) -> "type " ^ tvar ^ " = " ^ Types.string_of_typeML ty
  | ValDecl (var, exprML) ->
      " let " ^ var ^ " = " ^ Syntax.string_of_exprML exprML

let string_of_prog prog =
  String.concat "\n" ((List.map string_of_implem_decl) prog)

let extract_type_subst signature =
  let rec aux = function
    | [] -> []
    | TypeDecl (tvar, ty) :: l -> (tvar, ty) :: aux l
    | _ :: l -> aux l in
  Util.Pmap.list_to_pmap (aux signature)

let split_implem_decl_list implem_decl_l =
  let rec aux (val_decl_l, type_decl_l) = function
    | [] -> (val_decl_l, type_decl_l)
    | TypeDecl td :: implem_decl_l' ->
        aux (val_decl_l, td :: type_decl_l) implem_decl_l'
    | ValDecl vd :: implem_decl_l' ->
        aux (vd :: val_decl_l, type_decl_l) implem_decl_l' in
  aux ([], []) implem_decl_l

let split_signature_decl_list signature_decl_l =
  let rec aux (val_decl_l, type_decl_l) = function
    | [] -> (val_decl_l, type_decl_l)
    | PublicTypeDecl td :: signature_decl_l' ->
        aux (val_decl_l, td :: type_decl_l) signature_decl_l'
    | PrivateTypeDecl _ :: signature_decl_l' ->
        aux (val_decl_l, type_decl_l) signature_decl_l'
    | PublicValDecl vd :: signature_decl_l' ->
        aux (vd :: val_decl_l, type_decl_l) signature_decl_l' in
  aux ([], []) signature_decl_l

type comp_env = (Syntax.id * Syntax.exprML) list

let get_typed_comp_env implem_decl_l =
  let (val_decl_l, type_decl_l) = split_implem_decl_list implem_decl_l in
  let type_subst = Util.Pmap.list_to_pmap type_decl_l in
  let aux (var, expr) =
    let ty = Type_checker.typing_full type_subst expr in
    ((var, expr), (var, ty)) in
  let (comp_env, var_ctx_l) = List.split @@ List.map aux val_decl_l in
  (comp_env, Util.Pmap.list_to_pmap var_ctx_l)

let get_typed_int_env val_env sign_decl_l =
  let (var_ctx_l, type_decl_l) = split_signature_decl_list sign_decl_l in
  let _ = Util.Pmap.list_to_pmap type_decl_l in
  let aux (var, ty) =
    let value = Util.Pmap.lookup_exn var val_env in
    let nn = Syntax.name_of_id var in
    ((nn, value), (nn, ty)) in
  let (ienv_l, name_ctx_l) = List.split @@ List.map aux var_ctx_l in
  (Focusing.list_to_ienv ienv_l, Util.Pmap.list_to_pmap name_ctx_l)
