open Syntax
open Types
open Type_ctx

exception TypingError of string

let rec infer_type type_ctx expr =
  match expr with
  | Var x -> begin
      match Util.Pmap.lookup x type_ctx.var_ctx with
      | Some ty -> (ty, type_ctx)
      | None ->
          Util.Error.fail_error
            ("Error: the variable " ^ Syntax.string_of_id x
           ^ " is not defined in the environment "
            ^ Type_ctx.string_of_var_ctx type_ctx.var_ctx
            ^ " .")
    end
  | Constructor (cons, e) -> begin
      match Util.Pmap.lookup cons type_ctx.cons_ctx with
      | Some (TArrow (pty, ty)) ->
        let (pty', type_ctx') = infer_type type_ctx e in
          begin
            match unify_type type_ctx'.type_subst (pty, pty') with
            | Some (_, type_subst) -> (ty, Type_ctx.update_type_subst type_ctx' type_subst)
            | None -> 
                Util.Error.fail_error
                  ("Error typing "
                  ^ Syntax.string_of_term e
                  ^ ": " ^ string_of_typ pty' ^ " is not equal to "
                  ^ string_of_typ pty)
          end 
      | Some cty -> 
          Util.Error.fail_error
            ("Error typing: the type of the constructor " ^
            Syntax.string_of_constructor cons ^ " is " ^
            string_of_typ cty ^ " when it is expected to 
            be of the form a -> b.")
      | None ->
          Util.Error.fail_error
            ("Error: the constructor " ^ Syntax.string_of_constructor cons
            ^ " is not defined in the environment " 
            ^ Type_ctx.string_of_name_ctx type_ctx.name_ctx
            ^ " .")
      end
  | Name n -> begin
      match Util.Pmap.lookup n type_ctx.name_ctx with
      | Some ty -> (ty, type_ctx)
      | None ->
          Util.Error.fail_error
            ("Error: the name " ^ Names.string_of_name n
           ^ " is not defined in the environment "
            ^ Type_ctx.string_of_name_ctx type_ctx.name_ctx
            ^ " .")
    end
  | Loc l -> begin
      match Util.Pmap.lookup l type_ctx.loc_ctx with
      | Some ty -> (ty, type_ctx)
      | None ->
          Util.Error.fail_error
            ("Error: the location " ^ Syntax.string_of_loc l
           ^ " is not defined.")
    end
  | Unit -> (TUnit, type_ctx)
  | Int _ -> (TInt, type_ctx)
  | Bool _ -> (TBool, type_ctx)
  | BinaryOp (Plus, e1, e2)
  | BinaryOp (Minus, e1, e2)
  | BinaryOp (Mult, e1, e2)
  | BinaryOp (Div, e1, e2) -> begin
      try check_type_bin type_ctx TInt e1 e2 TInt
      with TypingError msg ->
        Util.Error.fail_error
          ("Error typing Arithmetic Operator "
          ^ Syntax.string_of_term expr
          ^ ": " ^ msg)
    end
  | BinaryOp (And, e1, e2) | BinaryOp (Or, e1, e2) ->
      check_type_bin type_ctx TBool e1 e2 TBool
  | BinaryOp (Equal, e1, e2)
  | BinaryOp (NEqual, e1, e2)
  | BinaryOp (Less, e1, e2)
  | BinaryOp (LessEq, e1, e2)
  | BinaryOp (Great, e1, e2)
  | BinaryOp (GreatEq, e1, e2) ->
      check_type_bin type_ctx TInt e1 e2 TBool
  | UnaryOp (Not, e) -> check_type type_ctx e TBool
  | If (e1, e2, e3) ->
      let (_, type_ctx1) = check_type type_ctx e1 TBool in
      let (ty2, type_ctx2) = infer_type type_ctx1 e2 in
      let (ty3, type_ctx3) = infer_type type_ctx2 e3 in
      begin
        match unify_type type_ctx3.type_subst (ty2, ty3) with
        | Some (ty, type_subst) ->
            Util.Debug.print_debug ("Typing If:" ^ string_of_typ ty);
            (ty, Type_ctx.update_type_subst type_ctx3 type_subst)
        | None ->
            Util.Error.fail_error
              ("Error typing "
              ^ Syntax.string_of_term expr
              ^ ": " ^ string_of_typ ty2 ^ " is not equal to "
              ^ string_of_typ ty3)
      end
  | Fun ((var, TUndef), e) ->
      let tvar = fresh_typevar () in
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx var tvar in
      let (ty2, type_ctx'') = infer_type type_ctx' e in
      let ty1' = Util.Pmap.lookup_exn var type_ctx''.var_ctx in
      (TArrow (ty1', ty2), type_ctx'')
  | Fun ((var, ty1), e) ->
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx var ty1 in
      let (ty2, type_ctx'') = infer_type type_ctx' e in
      let ty1' = Util.Pmap.lookup_exn var type_ctx''.var_ctx in
      (TArrow (ty1', ty2), type_ctx'')
  | Fix ((idfun, TUndef), (var, TUndef), e) ->
      let tvar1 = fresh_typevar () in
      let tvar2 = fresh_typevar () in
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx var tvar1 in
      let type_ctx' =
        Type_ctx.extend_var_ctx type_ctx' idfun (TArrow (tvar1, tvar2)) in
      let (rty, type_ctx'') = infer_type type_ctx' e in
      begin
        match
          ( Util.Pmap.lookup var type_ctx''.var_ctx,
            Util.Pmap.lookup idfun type_ctx''.var_ctx )
        with
        | (Some aty, Some fty) -> begin
            match unify_type type_ctx''.type_subst (fty, TArrow (aty, rty)) with
            | Some (ty, type_subst) ->
                (ty, Type_ctx.update_type_subst type_ctx'' type_subst)
            | None ->
                Util.Error.fail_error
                  ("Error typing "
                  ^ Syntax.string_of_term expr
                  ^ ": " ^ string_of_typ fty ^ " is not equal to "
                  ^ string_of_typ (TArrow (aty, rty)))
          end
        | _ -> failwith "Variables not found in type-checking. Please report."
      end
  | Fix ((idfun, TUndef), (var, aty), e) ->
      let tvar2 = fresh_typevar () in
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx var aty in
      let type_ctx' =
        Type_ctx.extend_var_ctx type_ctx' idfun (TArrow (aty, tvar2)) in
      let (rty, type_ctx'') = infer_type type_ctx' e in
      begin
        match Util.Pmap.lookup idfun type_ctx''.var_ctx with
        | Some fty -> begin
            match unify_type type_ctx''.type_subst (fty, TArrow (aty, rty)) with
            | Some (ty, type_subst) ->
                (ty, Type_ctx.update_type_subst type_ctx'' type_subst)
            | None ->
                Util.Error.fail_error
                  ("Error typing "
                  ^ Syntax.string_of_term expr
                  ^ ": " ^ string_of_typ fty ^ " is not equal to "
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
      let (rty, type_ctx'') = infer_type type_ctx' e in
      begin
        match unify_type type_ctx''.type_subst (fty, TArrow (aty, rty)) with
        | Some (ty, type_subst) ->
            (ty, Type_ctx.update_type_subst type_ctx'' type_subst)
        | None ->
            Util.Error.fail_error
              ("Error typing "
              ^ Syntax.string_of_term expr
              ^ ": " ^ string_of_typ fty ^ " is not equal to "
              ^ string_of_typ (TArrow (aty, rty)))
      end
  | Let (var, e1, e2) ->
      let (ty, type_ctx') = infer_type type_ctx e1 in
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx' var ty in
      infer_type type_ctx' e2
  | LetPair (var1, var2, e1, e2) ->
      let (ty, type_ctx') = infer_type type_ctx e1 in
      begin
        match ty with
        | TProd (ty1, ty2) ->
            let type_ctx'' = Type_ctx.extend_var_ctx type_ctx' var1 ty1 in
            let type_ctx'' = Type_ctx.extend_var_ctx type_ctx'' var2 ty2 in
            infer_type type_ctx'' e2
        | TVar tvar ->
            let tvar1 = fresh_typevar () in
            let tvar2 = fresh_typevar () in
            let type_ctx'' =
              extend_type_subst type_ctx' tvar (TProd (tvar1, tvar2)) in
            let type_ctx'' = Type_ctx.extend_var_ctx type_ctx'' var1 tvar1 in
            let type_ctx'' = Type_ctx.extend_var_ctx type_ctx'' var2 tvar2 in
            infer_type type_ctx'' e2
        | _ ->
            Util.Error.fail_error
              ("Error typing "
              ^ Syntax.string_of_term expr
              ^ " : " ^ string_of_typ ty ^ " is not a product type")
      end
  | App (e1, e2) ->
      let (aty, type_ctx') = infer_type type_ctx e2 in
      let (fty, type_ctx'') = infer_type type_ctx' e1 in
      let aty' = apply_type_subst aty type_ctx''.type_subst in
      Util.Debug.print_debug ("Typing App:" ^ Syntax.string_of_term expr);
      Util.Debug.print_debug
        (Syntax.string_of_term e1 ^ " is of type " ^ string_of_typ fty);
      Util.Debug.print_debug
        (Syntax.string_of_term e2 ^ " is of type " ^ string_of_typ aty'
       ^ " (was " ^ string_of_typ aty ^ ")");
      begin
        match fty with
        | TVar a ->
            let tvar = fresh_typevar () in
            let new_type_a = TArrow (aty', tvar) in
            let type_ctx''' =
              Type_ctx.extend_type_subst type_ctx'' a new_type_a in
            (tvar, type_ctx''')
        | TArrow (ty', rty) -> begin
            match unify_type type_ctx''.type_subst (ty', aty') with
            | Some (_, type_subst) ->
                let type_ctx''' =
                  Type_ctx.update_type_subst type_ctx'' type_subst in
                (rty, type_ctx''')
            | None ->
                Util.Error.fail_error
                  ("Error typing "
                  ^ Syntax.string_of_term expr
                  ^ ": " ^ string_of_typ ty' ^ " is not equal to "
                  ^ string_of_typ aty')
          end
        | _ ->
            Util.Error.fail_error
              ("Error typing "
              ^ Syntax.string_of_term expr
              ^ ": " ^ string_of_typ fty ^ " is not an arrow type")
      end
  | Seq (e1, e2) ->
      let (_, type_ctx') = check_type type_ctx e1 TUnit in
      infer_type type_ctx' e2
  | While (e1, e2) ->
      let (_, type_ctx') = check_type type_ctx e1 TBool in
      check_type type_ctx' e2 TUnit
  | Pair (e1, e2) ->
      let (ty1, type_ctx') = infer_type type_ctx e1 in
      let (ty2, type_ctx'') = infer_type type_ctx' e2 in
      (TProd (ty1, ty2), type_ctx'')
  | Newref (_, e) ->
      let (ty, type_ctx') = infer_type type_ctx e in
      (TRef ty, type_ctx')
  | Deref e ->
      let (ty, type_ctx') = infer_type type_ctx e in
      begin
        match ty with
        | TRef ty -> (ty, type_ctx')
        | _ ->
            Util.Error.fail_error
              ("Error typing "
              ^ Syntax.string_of_term expr
              ^ " : " ^ string_of_typ ty ^ " is not a ref type")
      end
  | Assign (e1, e2) ->
      let (ty1, type_ctx') = infer_type type_ctx e1 in
      let (ty2, type_ctx'') = infer_type type_ctx' e2 in
      begin
        match unify_type type_ctx''.type_subst (ty1, TRef ty2) with
        | Some (_, type_subst) ->
            (TUnit, Type_ctx.update_type_subst type_ctx'' type_subst)
        | None ->
            Util.Error.fail_error @@ "Error typing "
            ^ Syntax.string_of_term expr
            ^ " : " ^ string_of_typ ty1 ^ " is not unifiable with ref "
            ^ string_of_typ ty2
      end
  | Assert e ->
      let (ty, type_ctx') = infer_type type_ctx e in
      begin
        match ty with
        | TBool -> (TUnit, type_ctx')
        | _ ->
            Util.Error.fail_error
              ("Error typing "
              ^ Syntax.string_of_term expr
              ^ " : " ^ string_of_typ ty ^ " is not equal to bool.")
      end
  | Raise e ->
      let (ty, type_ctx') = infer_type type_ctx e in
      begin
        match ty with
        | TExn ->
            let tvar = fresh_typevar () in
            (tvar, type_ctx')
        | _ ->
            Util.Error.fail_error
              ("Error typing "
              ^ Syntax.string_of_term expr
              ^ " : " ^ string_of_typ ty ^ " is not equal to exn.")
      end
  | TryWith (e, handler_l) ->
      let (ty, type_ctx') = infer_type type_ctx e in
      let aux (ty, type_ctx) (Handler (pat, e_handler)) =
        let (ty', type_ctx') =
          begin
            match pat with
            | PatCons _ ->
                (* TODO: We should check that the constructor exists *)
                infer_type type_ctx e_handler
            | PatVar id ->
                let type_ctx' = Type_ctx.extend_var_ctx type_ctx id TExn in
                infer_type type_ctx' e_handler
          end in
        begin
          match unify_type type_ctx'.type_subst (ty, ty') with
          | Some (ty, type_subst) ->
              (ty, Type_ctx.update_type_subst type_ctx' type_subst)
          | None -> failwith "Type checking of try with not fully implemented."
        end in
      List.fold_left aux (ty, type_ctx') handler_l
  | Hole -> failwith "Error: The typechecker cannot type a hole."
  | Error ->
      let tvar = fresh_typevar () in
      (tvar, type_ctx)

and check_type type_ctx expr res_ty =
  let (ty, type_ctx') = infer_type type_ctx expr in
  match ty with
  | _ when ty = res_ty -> (ty, type_ctx')
  | TVar a -> (res_ty, Type_ctx.extend_type_subst type_ctx' a res_ty)
  | _ ->
      Util.Error.fail_error
        ("Error typing "
        ^ Syntax.string_of_term expr
        ^ " : " ^ string_of_typ ty ^ " is not equal to "
        ^ string_of_typ res_ty)

and check_type_bin type_ctx com_ty expr1 expr2 res_ty =
  let (ty1, type_ctx') = check_type type_ctx expr1 com_ty in
  let (ty2, type_ctx'') = check_type type_ctx' expr2 com_ty in
  Util.Debug.print_debug
    ("Typing binary operators "
    ^ Syntax.string_of_term expr1
    ^ " and "
    ^ Syntax.string_of_term expr2
    ^ " with " ^ string_of_typ com_ty ^ ":" ^ string_of_typ ty1 ^ " and  "
    ^ string_of_typ ty2);
  (res_ty, type_ctx'')

let infer_gen_type type_ctx expr =
  let (ty, type_ctx) = infer_type type_ctx expr in
  let tvar_l = get_tvars ty in
  (TForall (tvar_l, ty), type_ctx)

let typing_full type_subst expr =
  let lnames = Syntax.get_names expr in
  let name_ctx =
    Util.Pmap.list_to_pmap
    @@ List.map
         (fun n ->
           let tvar = fresh_typevar () in
           (n, tvar))
         lnames in
  let type_ctx =
    {
      var_ctx= Type_ctx.empty_var_ctx;
      loc_ctx= Type_ctx.empty_loc_ctx;
      name_ctx;
      cons_ctx = Type_ctx.empty_cons_ctx;
      type_subst;
    } in
  let (ty, _) = infer_type type_ctx expr in
  ty
