open Type_ctx

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

let build_name_ctx comp_env =
  let name_set =
    List.fold_left
      (fun n_set (_, expr) -> Syntax.get_new_names n_set expr)
      Syntax.empty_name_set comp_env in
  Util.Pmap.list_to_pmap
  @@ List.map
       (fun n ->
         let tvar = Types.fresh_typevar () in
         (n, tvar))
       name_set

let get_typed_comp_env implem_decl_l =
  let (val_decl_l, type_decl_l) = split_implem_decl_list implem_decl_l in
  let type_subst = Util.Pmap.list_to_pmap type_decl_l in
  let name_ctx = build_name_ctx val_decl_l in (* TODO: Should we also put domain of type_subst in name_ctx ?*)
  let type_ctx =
    {
      var_ctx= Type_ctx.empty_var_ctx;
      loc_ctx= Type_ctx.empty_loc_ctx;
      name_ctx;
      type_subst;
    } in
  let rec aux comp_env type_ctx = function
    | [] -> (comp_env, type_ctx)
    | (var, expr) :: val_decl_l ->
        let (ty, type_ctx') = Type_checker.infer_type type_ctx expr in
        let type_ctx'' = Type_ctx.extend_var_ctx type_ctx' var ty in
        aux ((var, expr) :: comp_env) type_ctx'' val_decl_l in
  aux [] type_ctx val_decl_l

let get_typed_int_env val_env sign_decl_l =
  let (var_ctx_l, type_decl_l) = split_signature_decl_list sign_decl_l in
  let _ = Util.Pmap.list_to_pmap type_decl_l in
  let aux (var, ty) =
    let value = Util.Pmap.lookup_exn var val_env in
    let nn = Syntax.fname_of_id var in
    ((nn, value), (nn, ty)) in
  let (ienv_l, name_ctx_l) = List.split @@ List.map aux var_ctx_l in
  (Focusing.list_to_ienv ienv_l, Util.Pmap.list_to_pmap name_ctx_l)
