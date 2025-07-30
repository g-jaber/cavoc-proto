open Syntax
open Types
open Type_ctx

exception TypingError of string

let rec infer_type type_ctx type_subst expr =
  match expr with
  | Var x -> begin
      match Util.Pmap.lookup x type_ctx.var_ctx with
      | Some ty -> (ty, type_subst)
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
          let (pty', type_subst') = infer_type type_ctx type_subst e in
          (*Should we instantiate pty' ? *)
          begin
            match mgu_type (Type_ctx.get_type_env type_ctx) (pty, pty') with
            | Some type_subst'' ->
                (ty, compose_type_subst type_subst'' type_subst')
            | None ->
                Util.Error.fail_error
                  ("Error typing " ^ Syntax.string_of_term e ^ ": "
                 ^ string_of_typ pty' ^ " is not equal to " ^ string_of_typ pty
                  )
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
            ^ Type_ctx.string_of_name_ctx type_ctx.name_ctx
            ^ " .")
    end
  | Name n -> begin
      match Util.Pmap.lookup n type_ctx.name_ctx with
      | Some ty -> (ty, type_subst)
      | None ->
          Util.Error.fail_error
            ("Error: the name " ^ Names.string_of_name n
           ^ " is not defined in the environment "
            ^ Type_ctx.string_of_name_ctx type_ctx.name_ctx
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
  | Unit -> (TUnit, type_subst)
  | Int _ -> (TInt, type_subst)
  | Bool _ -> (TBool, type_subst)
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
    let tsubst =
      check_type_bin type_ctx type_subst TInt e1 e2
    in (TBool,tsubst)
  | UnaryOp (Not, e) -> 
    let tsubst = check_type type_ctx type_subst e TBool
  in (TBool,tsubst)
  | If (e1, e2, e3) ->
      let type_subst1 = check_type type_ctx type_subst e1 TBool in
      let (ty2, type_subst2) = infer_type type_ctx type_subst1 e2 in
      let (ty3, type_subst3) = infer_type type_ctx type_subst2 e3 in
      (* Should we instantiate ty2 and ty3 ?*)
      begin
        match mgu_type (Type_ctx.get_type_env type_ctx) (ty2, ty3) with
        | Some type_subst' -> (ty3, compose_type_subst type_subst' type_subst3)
        | None ->
            Util.Error.fail_error
              ("Error typing " ^ Syntax.string_of_term expr ^ ": "
             ^ string_of_typ ty2 ^ " is not equal to " ^ string_of_typ ty3)
      end
  | Fun ((var, TUndef), e) ->
          Util.Debug.print_debug "THere ?";
      let tvar = fresh_typevar () in
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx var tvar in
      let (ty2, type_subst') = infer_type type_ctx' type_subst e in
      (TArrow (tvar, ty2), type_subst')
  | Fun ((var, ty1), e) ->
      Util.Debug.print_debug ("Here ? with " ^ (Syntax.string_of_term e));
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
      begin
        match mgu_type (Type_ctx.get_type_env type_ctx) (tvar2, rty) with
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
      begin
        match Util.Pmap.lookup idfun type_ctx'.var_ctx with
        | Some fty -> begin
            match mgu_type (Type_ctx.get_type_env type_ctx) (fty, TArrow (aty, rty)) with
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
      begin
        match mgu_type (Type_ctx.get_type_env type_ctx) (fty, TArrow (aty, rty)) with
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
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx var ty in
      infer_type type_ctx' type_subst' e2
  | LetPair (var1, var2, e1, e2) ->
      let (ty, type_subst') = infer_type type_ctx type_subst e1 in
      let tvar1 = fresh_typevar () in
      let tvar2 = fresh_typevar () in
      let type_ctx' = Type_ctx.extend_var_ctx type_ctx var1 tvar1 in
      let type_ctx'' = Type_ctx.extend_var_ctx type_ctx' var2 tvar2 in
      begin match mgu_type (Type_ctx.get_type_env type_ctx) (ty, TProd (tvar1,tvar2)) with
      | Some type_subst'' -> 
        let type_subst'' = compose_type_subst type_subst'' type_subst' in
        infer_type type_ctx'' type_subst'' e2
      | None ->             Util.Error.fail_error
              ("Error typing " ^ Syntax.string_of_term expr ^ " : "
             ^ string_of_typ ty ^ " is not a product type")
      end
  | App (e1, e2) ->
      let (aty, type_subst') = infer_type type_ctx type_subst e2 in
      let (fty, type_subst'') = infer_type type_ctx type_subst' e1 in
      let fty' = Types.apply_type_subst fty type_subst'' in
      let aty' = Types.apply_type_subst aty type_subst'' in
      Util.Debug.print_debug ("Typing App:" ^ Syntax.string_of_term expr);
      Util.Debug.print_debug
        (Syntax.string_of_term e1 ^ " is of type " ^ string_of_typ fty'
       ^ " (was " ^ string_of_typ fty ^ ")");
      Util.Debug.print_debug
        (Syntax.string_of_term e2 ^ " is of type " ^ string_of_typ aty'
       ^ " (was " ^ string_of_typ aty ^ ")");
      let tvar = fresh_typevar () in
      begin
        match mgu_type (Type_ctx.get_type_env type_ctx) (fty', TArrow (aty', tvar)) with
        | Some tsubst''' -> (tvar, tsubst''')
        | None ->
            Util.Error.fail_error
              ("Error typing " ^ Syntax.string_of_term expr ^ ": "
             ^ string_of_typ fty' ^ " is not equal to "
              ^ string_of_typ (TArrow (aty', tvar)))
      end
  | Seq (e1, e2) ->
      let type_subst' = check_type type_ctx type_subst e1 TUnit in
      infer_type type_ctx type_subst' e2
  | While (e1, e2) ->
      let type_subst' = check_type type_ctx type_subst e1 TBool in
      let type_subst'' = check_type type_ctx type_subst' e2 TUnit in
      (TUnit,type_subst'')
  | Pair (e1, e2) ->
      let (ty1, type_subst') = infer_type type_ctx type_subst e1 in
      let (ty2, type_subst'') = infer_type type_ctx type_subst' e2 in
      (TProd (ty1, ty2), type_subst'')
  | Newref (_, e) ->
      let (ty, type_subst') = infer_type type_ctx type_subst e in
      (TRef ty, type_subst')
  | Deref e ->
      let (ty, type_subst') = infer_type type_ctx type_subst e in
      begin
        match ty with
        | TRef ty -> (ty, type_subst')
        | _ ->
            Util.Error.fail_error
              ("Error typing " ^ Syntax.string_of_term expr ^ " : "
             ^ string_of_typ ty ^ " is not a ref type")
      end
  | Assign (e1, e2) ->
      let (ty1, type_subst') = infer_type type_ctx type_subst e1 in
      let (ty2, type_subst'') = infer_type type_ctx type_subst' e2 in
      begin
        match mgu_type (Type_ctx.get_type_env type_ctx) (ty1, TRef ty2) with
        | Some type_subst''' ->
            (TUnit, compose_type_subst type_subst''' type_subst'')
        | None ->
            Util.Error.fail_error @@ "Error typing "
            ^ Syntax.string_of_term expr ^ " : " ^ string_of_typ ty1
            ^ " is not unifiable with ref " ^ string_of_typ ty2
      end
  | Assert e ->
      let (ty, type_subst') = infer_type type_ctx type_subst e in
      begin
        match ty with
        | TBool -> (TUnit, type_subst')
        | _ ->
            Util.Error.fail_error
              ("Error typing " ^ Syntax.string_of_term expr ^ " : "
             ^ string_of_typ ty ^ " is not equal to bool.")
      end
  | Raise e ->
      let (ty, type_subst') = infer_type type_ctx type_subst e in
      begin
        match ty with
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
          begin
            match pat with
            | PatCons _ ->
                (* TODO: We should check that the constructor exists *)
                infer_type type_ctx type_subst e_handler
            | PatVar id ->
                let type_ctx' = Type_ctx.extend_var_ctx type_ctx id TExn in
                infer_type type_ctx' type_subst e_handler
          end in
        begin
          match mgu_type (Type_ctx.get_type_env type_ctx) (ty, ty') with
          | Some type_subst'' ->
              (ty, compose_type_subst type_subst'' type_subst')
          | None -> failwith "Type checking of try with not fully implemented."
        end in
      List.fold_left aux (ty, type_subst') handler_l
  | Hole -> failwith "Error: The typechecker cannot type a hole."
  | Error ->
      let tvar = fresh_typevar () in
      (tvar, type_subst)

and check_type type_ctx type_subst expr res_ty =
  let (ty, type_subst') = infer_type type_ctx type_subst expr in
  let ty_inst = Types.apply_type_subst ty type_subst' in
  match mgu_type (Type_ctx.get_type_env type_ctx) (ty_inst, res_ty) with
  | Some type_subst'' -> type_subst''
  | None ->
      Util.Error.fail_error
        ("Error typing " ^ Syntax.string_of_term expr ^ " : " ^ string_of_typ ty
       ^ " is not equal to " ^ string_of_typ res_ty)

and check_type_bin type_ctx type_subst com_ty expr1 expr2 =
  let type_subst' = check_type type_ctx type_subst expr1 com_ty in
  let type_subst'' = check_type type_ctx type_subst' expr2 com_ty in
  type_subst''

(*  
let infer_gen_type type_ctx type_subst expr =
  let (ty, type_subst') = infer_type type_ctx type_subst expr in
  let tvar_l = get_tvars ty in
  (TForall (tvar_l, ty), type_subst')
*)

let typing_expr type_ctx expr =
  let (ty, tsubst') = infer_type type_ctx Types.empty_type_subst expr in
  let ty' = Types.apply_type_subst ty tsubst' in
  let type_ctx' =  Type_ctx.apply_type_subst type_ctx tsubst' in
  (type_ctx',ty')