open Type_ctx

type signature_decl =
  | PrivateTypeDecl of Types.id
  | PublicTypeDecl of (Types.id * Types.typ)
  | PublicValDecl of (Syntax.id * Types.typ)
  | PublicExnDecl of (Syntax.constructor * Types.typ)

let string_of_signature_decl = function
  | PrivateTypeDecl tid -> "type " ^ tid
  | PublicTypeDecl (tid, ty) ->
      "type " ^ tid ^ " = " ^ Types.string_of_typ ty
  | PublicValDecl (var, ty) -> " val " ^ var ^ " : " ^ Types.string_of_typ ty
  | PublicExnDecl (c, param_ty) -> "exception " ^ Syntax.string_of_constructor c ^ " of " ^ Types.string_of_typ param_ty

let string_of_signature signature =
  String.concat "\n" ((List.map string_of_signature_decl) signature)

type implem_decl =
  | TypeDecl of (Types.id * Types.typ)
  | ValDecl of (Syntax.id * Syntax.term)
  | ExnDecl of (Syntax.constructor * Types.typ)

let string_of_implem_decl = function
  | TypeDecl (tid, ty) -> "type " ^ tid ^ " = " ^ Types.string_of_typ ty
  | ValDecl (var, term) ->
      " let " ^ var ^ " = " ^ Syntax.string_of_term term
  | ExnDecl (c, param_ty) -> "exception " ^ Syntax.string_of_constructor c ^ " of " ^ Types.string_of_typ param_ty

let string_of_prog prog =
  String.concat "\n" ((List.map string_of_implem_decl) prog)

let extract_type_subst signature =
  let rec aux = function
    | [] -> []
    | TypeDecl (tid, ty) :: l -> (tid, ty) :: aux l
    | _ :: l -> aux l in
  Util.Pmap.list_to_pmap (aux signature)

let split_implem_decl_list implem_decl_l =
  let rec aux (val_decl_l, type_decl_l, exn_l) = function
    | [] -> (val_decl_l, type_decl_l, exn_l)
    | TypeDecl td :: implem_decl_l' ->
        aux (val_decl_l, td :: type_decl_l, exn_l) implem_decl_l'
    | ValDecl vd :: implem_decl_l' ->
        aux (vd :: val_decl_l, type_decl_l, exn_l) implem_decl_l'
    | ExnDecl (c, param_ty) :: implem_decl_l' ->
        aux (val_decl_l, type_decl_l, (c,Types.TArrow (param_ty, Types.TExn)) :: exn_l) implem_decl_l' in
  aux ([], [], []) implem_decl_l

let split_signature_decl_list signature_decl_l =
  let rec aux (val_decl_l, type_decl_l, exn_l) = function
    | [] -> (val_decl_l, type_decl_l, exn_l)
    | PublicTypeDecl (tid, ty) :: signature_decl_l' ->
        Util.Debug.print_debug @@ "The type id " ^ tid
        ^ " is implemented publicly by the type " ^ Types.string_of_typ ty;
        aux (val_decl_l, (tid, ty) :: type_decl_l, exn_l) signature_decl_l'
    | PrivateTypeDecl _ :: signature_decl_l' ->
        aux (val_decl_l, type_decl_l, exn_l) signature_decl_l'
    | PublicValDecl vd :: signature_decl_l' ->
        aux (vd :: val_decl_l, type_decl_l, exn_l) signature_decl_l'
    | PublicExnDecl c :: signature_decl_l' ->
        aux (val_decl_l, type_decl_l, c :: exn_l) signature_decl_l' in
  aux ([], [], []) signature_decl_l

type comp_env = (Syntax.id * Syntax.term) list

let build_name_ctx comp_env =
  let name_set =
    List.fold_left
      (fun n_set (_, expr) -> Syntax.get_new_names n_set expr)
      Syntax.empty_name_set comp_env in
  Util.Pmap.list_to_pmap
  @@ List.map
       (fun n ->
         let tid = Types.fresh_typevar () in
         (n, tid))
       name_set

let get_typed_comp_env implem_decl_l =
  let (val_decl_l, type_decl_l, exn_l) = split_implem_decl_list implem_decl_l in
  let type_subst = Util.Pmap.list_to_pmap type_decl_l in
  let name_ctx = build_name_ctx val_decl_l in
  let cons_ctx = Util.Pmap.list_to_pmap exn_l in
  (* TODO: Should we also put domain of type_subst in name_ctx ?*)
  let type_ctx =
    {
      var_ctx= Type_ctx.empty_var_ctx;
      loc_ctx= Type_ctx.empty_loc_ctx;
      name_ctx;
      cons_ctx;
      type_subst;
    } in
  let rec aux comp_env type_ctx = function
    | [] -> 
      let name_ctx =
        Type_ctx.get_name_ctx type_ctx in
      (comp_env, name_ctx, cons_ctx)
    | (var, expr) :: val_decl_l ->
        let (ty, type_ctx') = Type_checker.infer_type type_ctx expr in
        let type_ctx'' = Type_ctx.extend_var_ctx type_ctx' var ty in
        aux ((var, expr) :: comp_env) type_ctx'' val_decl_l in
  aux [] type_ctx val_decl_l

let get_typed_val_env var_val_env sign_decl_l =
  let (var_ctx_l, type_decl_l, _) = split_signature_decl_list sign_decl_l in
  let type_env = Util.Pmap.list_to_pmap type_decl_l in
  let aux (var, ty) =
    let value = Util.Pmap.lookup_exn var var_val_env in
    let nn = Names.fname_of_id var in
    let ty' = Types.generalize_type @@ Types.apply_type_env ty type_env in
    (nn, value), (nn, ty') in
  let (val_env_l, name_ctx_l) = List.split @@ List.map aux var_ctx_l in
  let name_ctx = Util.Pmap.list_to_pmap name_ctx_l in
  let val_env = Util.Pmap.list_to_pmap val_env_l in
  (val_env, name_ctx)
  (* TODO: We should check that the signature and the implementation agree *)