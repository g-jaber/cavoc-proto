open Type_ctx

type signature_decl =
  | PrivateTypeDecl of Types.id
  | PublicTypeDecl of (Types.id * Types.typ)
  | PublicValDecl of (Syntax.id * Types.typ)
  | PublicExnDecl of (Syntax.constructor * Types.typ option)

let lift_exn_ty = function
  | None -> Types.TExn
  | Some param_ty -> Types.TArrow (param_ty, Types.TExn)

let string_of_signature_decl = function
  | PrivateTypeDecl tid -> "type " ^ tid
  | PublicTypeDecl (tid, ty) -> "type " ^ tid ^ " = " ^ Types.string_of_typ ty
  | PublicValDecl (var, ty) -> " val " ^ var ^ " : " ^ Types.string_of_typ ty
  | PublicExnDecl (c, ty) ->
      "exception "
      ^ Syntax.string_of_constructor c
      ^ " of "
      ^ Types.string_of_typ (lift_exn_ty ty)

let string_of_signature signature =
  String.concat "\n" ((List.map string_of_signature_decl) signature)

type implem_decl =
  | TypeDecl of (Types.id * Types.typ)
  | ValDecl of (Syntax.id * Syntax.term)
  | ExnDecl of (Syntax.constructor * Types.typ option)

let string_of_implem_decl = function
  | TypeDecl (tid, ty) -> "type " ^ tid ^ " = " ^ Types.string_of_typ ty
  | ValDecl (var, term) -> " let " ^ var ^ " = " ^ Syntax.string_of_term term
  | ExnDecl (c, None) -> "Exception " ^ Syntax.string_of_constructor c
  | ExnDecl (c, Some param_ty) ->
      "Exception "
      ^ Syntax.string_of_constructor c
      ^ " of "
      ^ Types.string_of_typ param_ty

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
    | ExnDecl (c, ty_opt) :: implem_decl_l' ->
        aux
          (val_decl_l, type_decl_l, (c, lift_exn_ty ty_opt) :: exn_l)
          implem_decl_l' in
  aux ([], [], []) implem_decl_l

(* split_signature_decl_list return a quadruple formed by
- a list of public variables with their types
- a list of abstract type (i.e. type identifiers whose type declaration is not provided)
- a list of public type declarations
- a list of exception declarations
*)
let split_signature_decl_list signature_decl_l =
  let rec aux (var_decl_l, type_priv_decl_l, type_publ_decl_l, exn_l) = function
    | [] -> (var_decl_l, type_priv_decl_l, type_publ_decl_l, exn_l)
    | PublicTypeDecl (tid, ty) :: signature_decl_l' ->
        Util.Debug.print_debug @@ "The type id " ^ tid
        ^ " is implemented publicly by the type " ^ Types.string_of_typ ty;
        aux
          (var_decl_l, type_priv_decl_l, (tid, ty) :: type_publ_decl_l, exn_l)
          signature_decl_l'
    | PrivateTypeDecl tid :: signature_decl_l' ->
        aux
          (var_decl_l, tid :: type_priv_decl_l, type_publ_decl_l, exn_l)
          signature_decl_l'
    | PublicValDecl vd :: signature_decl_l' ->
        aux
          (vd :: var_decl_l, type_priv_decl_l, type_publ_decl_l, exn_l)
          signature_decl_l'
    | PublicExnDecl (c, ty_opt) :: signature_decl_l' ->
        aux
          ( var_decl_l,
            type_priv_decl_l,
            type_publ_decl_l,
            (c, lift_exn_ty ty_opt) :: exn_l )
          signature_decl_l' in
  aux ([], [], [], []) signature_decl_l

type comp_env = (Syntax.id * Syntax.term) list

let rec type_priv_included implem_type_decls = function
  | [] -> ()
  | tid :: tid_l -> begin
      match Util.Pmap.lookup tid implem_type_decls with
      | None ->
          Util.Error.fail_error
            ("Error: the type declaration " ^ tid
           ^ " is not present in the implementation.")
      | Some _ -> type_priv_included implem_type_decls tid_l
    end

let rec type_decl_coincide implem_type_decls = function
  | [] -> ()
  | (tid, ty) :: sign_type_decl_l -> begin
      match Util.Pmap.lookup tid implem_type_decls with
      | None ->
          Util.Error.fail_error
            ("Error: the type declaration " ^ tid
           ^ " is not present in the implementation.")
      | Some ty' when ty = ty' ->
          type_decl_coincide implem_type_decls sign_type_decl_l
      | Some ty' ->
          Util.Error.fail_error
            ("Error: the type declaration of " ^ tid
           ^ " does not match between the signature (" ^ Types.string_of_typ ty'
           ^ ") and its implementation (" ^ Types.string_of_typ ty ^ ")")
    end

let rec exn_included implem_exns = function
  | [] -> ()
  | (c, ty) :: exn_l -> begin
      match Util.Pmap.lookup c implem_exns with
      | None ->
          Util.Error.fail_error
            ("Error: the exception declaration " ^ c
           ^ " is not present in the implementation.")
      | Some ty' when ty = ty' -> exn_included implem_exns exn_l
      | Some ty' ->
          Util.Error.fail_error
            ("Error: the type of the exception declaration " ^ c
           ^ " does not match between the signature (" ^ Types.string_of_typ ty'
           ^ ") and its implementation (" ^ Types.string_of_typ ty ^ ")")
    end

let rec var_decl_included comp_decl_l = function
  | [] -> ()
  | (x, _) :: var_decl_l ->
      if List.mem_assoc x comp_decl_l then
        var_decl_included comp_decl_l var_decl_l
      else
        Util.Error.fail_error
          ("Error: the variable declaration " ^ x
         ^ " is not present in the implementation.")

let typing_decl_l type_ctx var_decls comp_decl_l =
  let rec aux comp_env type_ctx = function
    | [] ->
        let name_ctx = Type_ctx.get_name_ctx type_ctx in
        (comp_env, name_ctx)
    | (var, expr) :: val_decl_l -> begin
        match Util.Pmap.lookup var var_decls with
        | None ->
            let (type_ctx', ty) = Type_checker.typing_expr type_ctx expr in
            let type_ctx'' = Type_ctx.extend_var_ctx type_ctx' var ty in
            aux ((var, expr) :: comp_env) type_ctx'' val_decl_l
        | Some ty ->
            let (type_ctx', ty') = Type_checker.checking_expr type_ctx expr ty in
            let type_ctx'' = Type_ctx.extend_var_ctx type_ctx' var ty' in
            aux ((var, expr) :: comp_env) type_ctx'' val_decl_l
      end in
  aux [] type_ctx comp_decl_l

let get_typed_comp_env implem_decl_l sign_decl_l =
  let (comp_decl_l, implem_type_decl_l, implem_exn_l) =
    split_implem_decl_list implem_decl_l in
  let (var_decl_l, type_priv_decl_l, type_publ_decl_l, sign_exn_l) =
    split_signature_decl_list sign_decl_l in
  let type_env = Util.Pmap.list_to_pmap implem_type_decl_l in
  let cons_ctx = Util.Pmap.list_to_pmap implem_exn_l in
  var_decl_included comp_decl_l var_decl_l;
  type_priv_included type_env type_priv_decl_l;
  type_decl_coincide type_env type_publ_decl_l;
  exn_included cons_ctx sign_exn_l;
  let name_ctxO = Namectx.Namectx.empty in
  let var_decls = Util.Pmap.list_to_pmap var_decl_l in
  (* TODO: Should we also put domain of type_env in name_ctx ?*)
  let type_ctx =
    {
      var_ctx= Type_ctx.empty_var_ctx;
      loc_ctx= Type_ctx.empty_loc_ctx;
      name_ctx= name_ctxO;
      cons_ctx;
      type_env;
    } in
  let (comp_env, name_ctxO') = typing_decl_l type_ctx var_decls comp_decl_l in
  (comp_env, name_ctxO', cons_ctx)

let get_typed_val_env var_val_env sign_decl_l =
  let (var_ctx_l, _, type_publ_decl_l, _) =
    split_signature_decl_list sign_decl_l in
  let type_env = Util.Pmap.list_to_pmap type_publ_decl_l in
  let rec partition_env (((ienvf, ienvp), (fnamectx, pnamectx)) as acc) =
    function
    | [] -> acc
    | (var, ty) :: tl -> begin
        let value = Util.Pmap.lookup_exn var var_val_env in
        let ty' = Types.generalize_type @@ Types.apply_type_env ty type_env in
        (* We might have to switch this generalization with the match below*)
        match ty' with
        | Types.TArrow _ | Types.TForall _ ->
            let (fn, fnamectx') = Namectx.FNamectx.add_fresh fnamectx var ty' in
            let nval = Syntax.force_negative_val value in
            let ienvf' = Ienv.IEnvF.add_last_check ienvf fn nval in
            partition_env ((ienvf', ienvp), (fnamectx', pnamectx)) tl
        | Types.TId _ | Types.TName _ ->
            let (pn, pnamectx') = Namectx.PNamectx.add_fresh pnamectx var ty' in
            let nval = Syntax.force_negative_val value in
            let ienvp' = Ienv.IEnvP.add_last_check ienvp pn nval in
            partition_env ((ienvf, ienvp'), (fnamectx, pnamectx')) tl
        | _ ->
            Util.Debug.print_debug
              ("The identifier " ^ Syntax.string_of_id var
             ^ " is not included in the environment because it is of \
                non-negative type " ^ Types.string_of_typ ty');
            partition_env acc tl
      end in

  partition_env (Ienv.IEnv.empty, Namectx.Namectx.empty) var_ctx_l
