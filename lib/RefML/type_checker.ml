open Syntax
open Types
open Type_ctx

exception TypingError of string

let rec get_type_from_tid ty type_ctx = match ty with 
        | TId id -> get_type_from_tid (Util.Pmap.lookup_exn id (Type_ctx.get_type_env type_ctx)) type_ctx
        | ty -> ty

let rec infer_type type_ctx type_subst expr =
  match expr with
  | Var x -> begin
      match Util.Pmap.lookup x type_ctx.var_ctx with
      | Some ty ->
          let ty' = Types.refresh_forall ty in
          (ty', type_subst)
      | None ->
          Util.Error.fail_error
            ("Error: the variable " ^ Syntax.string_of_id x
           ^ " is not defined in the environment "
            ^ Type_ctx.string_of_var_ctx type_ctx.var_ctx
            ^ " .")
    end
  | Constructor (cons, Some e) -> begin
      match Util.Pmap.lookup cons type_ctx.cons_ctx with
      | Some (TArrow (pty, ty)) ->
          let (pty', type_subst') = infer_type type_ctx type_subst e in
          (*Should we instantiate pty' ? *)
          begin match mgu_type (Type_ctx.get_type_env type_ctx) (pty, pty') with
          | Some type_subst'' ->
              (ty, compose_type_subst type_subst'' type_subst')
          | None ->
              Util.Error.fail_error
                ("Error typing " ^ Syntax.string_of_term e ^ ": "
               ^ string_of_typ pty' ^ " is not equal to " ^ string_of_typ pty)
          end
      | Some cty ->
          Util.Error.fail_error
            ("Error typing: the type of the constructor "
            ^ Syntax.string_of_constructor cons
            ^ " is " ^ string_of_typ cty
            ^ " when it is expected to \n            be of the form a -> b.")
      | None ->
          Util.Error.fail_error
            ("Error: the constructor "
            ^ Syntax.string_of_constructor cons
            ^ " is not defined in the environment "
            ^ Type_ctx.string_of_cons_ctx type_ctx.cons_ctx
            ^ " .")
    end
  | Constructor(_cons, None) -> failwith "Empty constructor not implemented yet (infer_type)"
  | Name n -> begin
      match Namectx.Namectx.lookup_exn (Type_ctx.get_name_ctx type_ctx) n with
      | ty -> (ty, type_subst)
      | exception Not_found ->
          Util.Error.fail_error
            ("Error: the name " ^ Names.string_of_name n
           ^ " is not defined in the environment "
            ^ Namectx.Namectx.to_string type_ctx.name_ctx
            ^ " .")
    end
  | Loc l -> begin
      match Util.Pmap.lookup l type_ctx.loc_ctx with
      | Some ty -> (ty, type_subst)
      | None ->
          Util.Error.fail_error
            ("Error: the location " ^ Syntax.string_of_loc l
           ^ " is not defined.")
    end
  | Symbolic _ -> (TBool, type_subst) (* TODO: fix this, Symbolic should not appear here anyway *)
  | Unit -> (TUnit, type_subst)
  | Int _ -> (TInt, type_subst)
  | Bool _ -> (TBool, type_subst)
  | Record fields ->
      let inference_step (type_subst, acc) (id, term) = 
        let (ty, type_subst') = infer_type type_ctx type_subst term in 
        (type_subst', Util.Pmap.add (id, ty) acc)
      in
      let (final_subst, inferred_fields) = 
        Util.Pmap.fold inference_step (type_subst, Util.Pmap.empty ) fields
      in (TRecord inferred_fields, final_subst)
  | Projection (term, field_name) -> (
      let get_tid_from_field field = (
        let id_ref = Util.Pmap.lookup field (Type_ctx.get_field_ctx type_ctx) in
        match id_ref with 
        | Some type_id -> Types.TId type_id
        | None -> Util.Error.fail_error (
            "Error typing " ^ Syntax.string_of_term (Projection (term, field_name)) ^ " : "
            ^ "Unbound record field " ^ Syntax.string_of_id field
        )
      ) in
      let get_associated_tid term id = (
        let rty, type_subst' = infer_type type_ctx type_subst term in 
        let rty' = Types.apply_type_subst rty type_subst' in 
        let associated_tid = get_tid_from_field id in
        (* Ensure that the infered field type correspond to the declared type in signature *)
        begin match mgu_type (Type_ctx.get_type_env type_ctx) (rty', associated_tid) with
        | Some type_subst'' -> (associated_tid, compose_type_subst type_subst'' type_subst')
        | None -> Util.Error.fail_error (
            "Error typing " ^ Syntax.string_of_term (Projection (term, id)) ^ " : "
            ^ "Unbound record field " ^ Syntax.string_of_id id
        )
        end
      ) in
      let get_type_from_field_name ty field_name =
        let ty' = get_type_from_tid ty type_ctx in
        match ty' with
        | TRecord fields -> Util.Pmap.lookup field_name fields
        | _ -> Util.Error.fail_error (
            "Error typing " ^ Syntax.string_of_term (Projection (term, field_name)) ^ " : "
            ^ Syntax.string_of_term term ^ " is not a Record type"
        )
      in
      let associated_tid, type_subst = get_associated_tid term field_name in
      let inferred_type = get_type_from_field_name associated_tid field_name in
      match inferred_type with 
      | Some ty -> (ty, type_subst)
      | None -> Util.Error.fail_error (
            "Error typing " ^ Syntax.string_of_term (Projection (term, field_name)) ^ " : "
            ^ "Unbound record field " ^ Syntax.string_of_id field_name
        )
  )
  | BinaryOp (Plus, e1, e2)
  | BinaryOp (Minus, e1, e2)
  | BinaryOp (Mult, e1, e2)
  | BinaryOp (Div, e1, e2) -> begin
      let tsubst =
        try check_type_bin type_ctx type_subst TInt e1 e2
        with TypingError msg ->
          Util.Error.fail_error
            ("Error typing Arithmetic Operator " ^ Syntax.string_of_term expr
           ^ ": " ^ msg) in
      (TInt, tsubst)
    end
  | BinaryOp (And, e1, e2) | BinaryOp (Or, e1, e2) ->
      let tsubst = check_type_bin type_ctx type_subst TBool e1 e2 in
      (TBool, tsubst)
  | BinaryOp (Equal, e1, e2)
  | BinaryOp (NEqual, e1, e2)
  | BinaryOp (Less, e1, e2)
  | BinaryOp (LessEq, e1, e2)
  | BinaryOp (Great, e1, e2)
  | BinaryOp (GreatEq, e1, e2) ->
      let tsubst = check_type_bin type_ctx type_subst TInt e1 e2 in
      (TBool, tsubst)
  | UnaryOp (Not, e) ->
      let tsubst = check_type type_ctx type_subst e TBool in
      (TBool, tsubst)
  | If (e1, e2, e3) ->
      let type_subst1 = check_type type_ctx type_subst e1 TBool in
      let (ty2, type_subst2) = infer_type type_ctx type_subst1 e2 in
      let (ty3, type_subst3) = infer_type type_ctx type_subst2 e3 in
      (* Should we instantiate ty2 and ty3 ?*)
      begin match mgu_type (Type_ctx.get_type_env type_ctx) (ty2, ty3) with
      | Some type_subst' -> (ty3, compose_type_subst type_subst' type_subst3)
      | None ->
          Util.Error.fail_error
            ("Error typing " ^ Syntax.string_of_term expr ^ ": "
           ^ string_of_typ ty2 ^ " is not equal to " ^ string_of_typ ty3)
      end
  | Fun ((var, TUndef), e) ->
      let tvar = fresh_typevar () in
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx var tvar in
      let (ty2, type_subst') = infer_type type_ctx' type_subst e in
      Util.Debug.print_debug @@ "The current type substitution is :"
      ^ Types.string_of_type_subst type_subst';
      (TArrow (tvar, ty2), type_subst')
  | Fun ((var, ty1), e) ->
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx var ty1 in
      let (ty2, type_subst') = infer_type type_ctx' type_subst e in
      (TArrow (ty1, ty2), type_subst')
  | Fix ((idfun, TUndef), (var, TUndef), e) ->
      let tvar1 = fresh_typevar () in
      let tvar2 = fresh_typevar () in
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx var tvar1 in
      let type_ctx'' =
        Type_ctx.extend_var_ctx type_ctx' idfun (TArrow (tvar1, tvar2)) in
      let (rty, type_subst') = infer_type type_ctx'' type_subst e in
      begin match mgu_type (Type_ctx.get_type_env type_ctx) (tvar2, rty) with
      | Some type_subst'' ->
          (TArrow (tvar1, rty), compose_type_subst type_subst'' type_subst')
      | None ->
          Util.Error.fail_error
            ("Error typing " ^ Syntax.string_of_term expr ^ ": "
           ^ string_of_typ tvar2 ^ " is not equal to " ^ string_of_typ rty)
      end
  | Fix ((idfun, TUndef), (var, aty), e) ->
      let tvar2 = fresh_typevar () in
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx var aty in
      let type_ctx' =
        Type_ctx.extend_var_ctx type_ctx' idfun (TArrow (aty, tvar2)) in
      let (rty, type_subst') = infer_type type_ctx' type_subst e in
      begin match Util.Pmap.lookup idfun type_ctx'.var_ctx with
      | Some fty -> begin
          match
            mgu_type (Type_ctx.get_type_env type_ctx) (fty, TArrow (aty, rty))
          with
          | Some type_subst'' ->
              (TArrow (aty, rty), compose_type_subst type_subst'' type_subst')
          | None ->
              Util.Error.fail_error
                ("Error typing " ^ Syntax.string_of_term expr ^ ": "
               ^ string_of_typ fty ^ " is not equal to "
                ^ string_of_typ (TArrow (aty, rty)))
        end
      | None ->
          failwith
            ("Variable " ^ Syntax.string_of_id idfun
           ^ " not found type-checking. Please report.")
      end
  | Fix ((idfun, fty), (var, aty), e) ->
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx var aty in
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx' idfun fty in
      let (rty, type_subst') = infer_type type_ctx' type_subst e in
      begin match
        mgu_type (Type_ctx.get_type_env type_ctx) (fty, TArrow (aty, rty))
      with
      | Some type_subst'' ->
          (TArrow (aty, rty), compose_type_subst type_subst'' type_subst')
      | None ->
          Util.Error.fail_error
            ("Error typing " ^ Syntax.string_of_term expr ^ ": "
           ^ string_of_typ fty ^ " is not equal to "
            ^ string_of_typ (TArrow (aty, rty)))
      end
  | Let (var, e1, e2) ->
      let (ty, type_subst') = infer_type type_ctx type_subst e1 in
      let ty' = Types.apply_type_subst ty type_subst' in
      let ty_gen = Types.generalize_type ty' in
      Util.Debug.print_debug
        ("We have " ^ Syntax.string_of_term e1 ^ " of type "
       ^ Types.string_of_typ ty
       ^ " before substitution and\n      generalization, and "
       ^ Types.string_of_typ ty_gen ^ " after.");
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx var ty_gen in
      infer_type type_ctx' type_subst' e2
  | LetPair (var1, var2, e1, e2) ->
      let (ty, type_subst') = infer_type type_ctx type_subst e1 in
      let tvar1 = fresh_typevar () in
      let tvar2 = fresh_typevar () in
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx var1 tvar1 in
      let type_ctx'' = Type_ctx.extend_var_ctx type_ctx' var2 tvar2 in
      begin match
        mgu_type (Type_ctx.get_type_env type_ctx) (ty, TProd (tvar1, tvar2))
      with
      | Some type_subst'' ->
          let type_subst'' = compose_type_subst type_subst'' type_subst' in
          infer_type type_ctx'' type_subst'' e2
      | None ->
          Util.Error.fail_error
            ("Error typing " ^ Syntax.string_of_term expr ^ " : "
           ^ string_of_typ ty ^ " is not a product type")
      end
  | App (e1, e2) ->
      let (fty, type_subst') = infer_type type_ctx type_subst e1 in
      let (aty, type_subst'') = infer_type type_ctx type_subst' e2 in
      let fty' = Types.apply_type_subst fty type_subst'' in
      let aty' = Types.apply_type_subst aty type_subst'' in
      Util.Debug.print_debug
        (Syntax.string_of_term e1 ^ " is of type " ^ string_of_typ fty'
       ^ " (was " ^ string_of_typ fty ^ ")");
      Util.Debug.print_debug
        (Syntax.string_of_term e2 ^ " is of type " ^ string_of_typ aty'
       ^ " (was " ^ string_of_typ aty ^ ")");
      let tvar = fresh_typevar () in
      begin match
        mgu_type (Type_ctx.get_type_env type_ctx) (fty', TArrow (aty', tvar))
      with
      | Some tsubst''' -> (tvar, compose_type_subst tsubst''' type_subst'')
      | None ->
          Util.Error.fail_error
            ("Error typing " ^ Syntax.string_of_term expr ^ ": "
           ^ string_of_typ fty' ^ " is not equal to "
            ^ string_of_typ (TArrow (aty', tvar)))
      end
  | Seq (e1, e2) ->
      let type_subst' = check_type type_ctx type_subst e1 TUnit in
      Util.Debug.print_debug @@ "In Seq, the current type substitution is : "
      ^ string_of_type_subst type_subst';
      infer_type type_ctx type_subst' e2
  | While (e1, e2) ->
      let type_subst' = check_type type_ctx type_subst e1 TBool in
      let type_subst'' = check_type type_ctx type_subst' e2 TUnit in
      (TUnit, type_subst'')
  | Pair (e1, e2) ->
      let (ty1, type_subst') = infer_type type_ctx type_subst e1 in
      let (ty2, type_subst'') = infer_type type_ctx type_subst' e2 in
      Util.Debug.print_debug @@ "In Pair, the current type substitution is : "
      ^ string_of_type_subst type_subst'';
      (TProd (ty1, ty2), type_subst'')
  | Newref (_, e) ->
      let (ty, type_subst') = infer_type type_ctx type_subst e in
      (TRef ty, type_subst')
  | Deref e ->
      let (ty, type_subst') = infer_type type_ctx type_subst e in
      begin match ty with
      | TRef ty -> (ty, type_subst')
      | _ ->
          Util.Error.fail_error
            ("Error typing " ^ Syntax.string_of_term expr ^ " : "
           ^ string_of_typ ty ^ " is not a ref type")
      end
  | Assign (e1, e2) ->
      let (ty1, type_subst') = infer_type type_ctx type_subst e1 in
      let (ty2, type_subst'') = infer_type type_ctx type_subst' e2 in
      begin match mgu_type (Type_ctx.get_type_env type_ctx) (ty1, TRef ty2) with
      | Some type_subst''' ->
          Util.Debug.print_debug
            ("We get an assign with " ^ Syntax.string_of_term e1 ^ " of type "
           ^ Types.string_of_typ ty1 ^ " and " ^ Syntax.string_of_term e2
           ^ " of type " ^ Types.string_of_typ ty2);
          let type_subst_final = compose_type_subst type_subst''' type_subst'' in
          Util.Debug.print_debug @@ "The current type substitution is : "
          ^ string_of_type_subst type_subst_final;
          (TUnit, type_subst_final)
      | None ->
          Util.Error.fail_error @@ "Error typing " ^ Syntax.string_of_term expr
          ^ " : " ^ string_of_typ ty1 ^ " is not unifiable with ref "
          ^ string_of_typ ty2
      end
  | Assert e ->
      let type_subst' = check_type type_ctx type_subst e TBool in
      (TUnit, type_subst')
  | Raise e ->
      let (ty, type_subst') = infer_type type_ctx type_subst e in
      begin match ty with
      | TExn ->
          let tvar = fresh_typevar () in
          (tvar, type_subst')
      | _ ->
          Util.Error.fail_error
            ("Error typing " ^ Syntax.string_of_term expr ^ " : "
           ^ string_of_typ ty ^ " is not equal to exn.")
      end
  | TryWith (e, handler_l) ->
      let (ty, type_subst') = infer_type type_ctx type_subst e in
      let aux (ty, type_subst) (Handler (pat, e_handler)) =
        let (ty', type_subst') =
          begin match pat with
          | PatCons _ ->
              (* TODO: We should check that the constructor exists *)
              infer_type type_ctx type_subst e_handler
          | PatVar id ->
              let type_ctx' = Type_ctx.extend_var_ctx type_ctx id TExn in
              infer_type type_ctx' type_subst e_handler
          end in
        begin match mgu_type (Type_ctx.get_type_env type_ctx) (ty, ty') with
        | Some type_subst'' -> (ty, compose_type_subst type_subst'' type_subst')
        | None -> failwith "Type checking of try with not fully implemented."
        end in
      List.fold_left aux (ty, type_subst') handler_l
  | Hole -> failwith "Error: The typechecker cannot type a hole."
  | Error ->
      let tvar = fresh_typevar () in
      (tvar, type_subst)
  | Match _ -> failwith "Match expression are not yet supported (infer_type)"
and check_type type_ctx type_subst expr res_ty =
  let (ty, type_subst') = infer_type type_ctx type_subst expr in
  let ty_inst = Types.apply_type_subst ty type_subst' in
  match mgu_type (Type_ctx.get_type_env type_ctx) (ty_inst, res_ty) with
  | Some type_subst'' -> compose_type_subst type_subst'' type_subst'
  | None ->
      Util.Error.fail_error
        ("Error typing " ^ Syntax.string_of_term expr ^ " : " ^ string_of_typ ty
       ^ " is not equal to " ^ string_of_typ res_ty)

and check_type_bin type_ctx type_subst com_ty expr1 expr2 =
  let type_subst' = check_type type_ctx type_subst expr1 com_ty in
  let type_subst'' = check_type type_ctx type_subst' expr2 com_ty in
  type_subst''

let typing_expr type_ctx expr =
  let (ty, tsubst) = infer_type type_ctx Types.empty_type_subst expr in
  let ty' = Types.apply_type_subst ty tsubst in
  let ty_gen = Types.generalize_type ty' in
  let type_ctx' = Type_ctx.apply_type_subst type_ctx tsubst in
  (type_ctx', ty_gen)

let checking_expr type_ctx expr ty =
  let tsubst = check_type type_ctx Types.empty_type_subst expr ty in
  let ty' = Types.apply_type_subst ty tsubst in
  let ty_gen = Types.generalize_type ty' in
  let type_ctx' = Type_ctx.apply_type_subst type_ctx tsubst in
  (type_ctx', ty_gen)
