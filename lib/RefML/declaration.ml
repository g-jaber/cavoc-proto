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

let get_ienv implem_decl_l =
  let (val_decl_l, type_decl_l) = split_implem_decl_list implem_decl_l in
  let tsubst = Util.Pmap.list_to_pmap type_decl_l in
  let aux (var, expr) =
    let ty = Type_checker.typing_full tsubst expr in
    let nn = Syntax.name_of_id var in
    ((nn, expr), (nn, ty)) in
  let (tval_l, tnn_l) = List.split @@ List.map aux val_decl_l in
  (Focusing.list_to_ienv tval_l, Util.Pmap.list_to_pmap tnn_l)
