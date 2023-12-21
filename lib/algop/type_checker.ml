open Syntax
open Types

exception TypingError of string

let rec infer_vtype type_ctx = function
  | Var id -> begin
      match VarCtx.find_opt id type_ctx.var_ctx with 
      | Some ty -> (ty, type_ctx)
      | None ->     
        failwith 
        (*Util.Error.fail_error *)
          ("Error: the variable " ^ Syntax.string_of_id id
         ^ " is not defined in the environment "
          (*^ Type_ctx.string_of_var_ctx type_ctx.var_ctx *)
          ^ " .")
      end
  | Constructor (cons, _) -> begin
    (* must type_check values *) 
    match ConsCtx.find_opt cons type_ctx.cons_ctx with
      | Some Tcons (_, ty) -> (ty, type_ctx)
      | Some _ -> failwith "Constructor must be of type ..." 
      | None -> 
        failwith 
        (*Util.Error.fail_error *)
          ("Error: the constructor " ^ Syntax.string_of_constructor cons
         ^ " is not defined in the environment "
          (*^ Type_ctx.string_of_var_ctx type_ctx.var_ctx *)
          ^ " .")
  end
  | Name n -> begin
      match NameCtx.find_opt n type_ctx.name_ctx with 
      | Some ty -> (ty, type_ctx)
      | None ->     
        failwith 
        (*Util.Error.fail_error *)
          ("Error: the name " ^ Names.string_of_name n
         ^ " is not defined in the environment "
          (*^ Type_ctx.string_of_var_ctx type_ctx.var_ctx *)
          ^ " .")
      end
  | Unit -> (TUnit, type_ctx)
  | Int _ -> (TInt, type_ctx)
  | Bool _ -> (TBool, type_ctx)
  | Binop (Plus, n1, n2)
  | Binop (Minus, n1, n2)
  | Binop (Mult, n1, n2)
  | Binop (Div, n1, n2) as val' -> begin
      try check_type_bin type_ctx TInt n1 n2 TInt
      with TypingError msg ->
        Util.Error.fail_error
          ("Error typing Arithmetic Operator "
          ^ Syntax.string_of_value val'
          ^ ": " ^ msg)
    end
  | Binop (And, e1, e2) | Binop (Or, e1, e2) ->
      check_type_bin type_ctx TBool e1 e2 TBool
  | Binop (Equal, e1, e2)
  | Binop (NEqual, e1, e2)
  | Binop (Less, e1, e2)
  | Binop (LessEq, e1, e2)
  | Binop (Great, e1, e2)
  | Binop (GreatEq, e1, e2) ->
        check_type_bin type_ctx TInt e1 e2 TBool
  | Record labelled_vals -> 
    let labelled_typs = List.fold_left 
     (fun l (lab, value) -> let (ty, _) =  infer_vtype type_ctx value in 
      (lab, ty)::l) [] labelled_vals in 
      (TRecord labelled_typs, type_ctx) 
  | Variant (lab, value) -> 
    let (vty, type_ctx') = infer_vtype type_ctx value in 
    (TVariant [(lab, vty)], type_ctx')
  | Lambda ((_, vty), comp) -> 
    let (cty, type_ctx') = infer_ctype type_ctx comp in 
    (TArrow (vty, cty), type_ctx')
      
and infer_ptype type_ctx = function
  | PatVar id -> 
      let ty = fresh_typevar() in (ty, Types.extend_var_ctx type_ctx id ty)
  | PatCons (cons, _) -> begin 
    match ConsCtx.find_opt cons type_ctx.cons_ctx with
      | Some Tcons (_, ty) -> (ty, type_ctx)
      | Some _ -> failwith "Constructor must be of type ..." 
      | None -> 
        failwith 
        (*Util.Error.fail_error *)
          ("Error: the constructor " ^ Syntax.string_of_constructor cons
         ^ " is not defined in the environment "
          (*^ Type_ctx.string_of_var_ctx type_ctx.var_ctx *)
          ^ " .")
  end
  | PatName n -> begin
      match NameCtx.find_opt n type_ctx.name_ctx with 
      | Some ty -> (ty, type_ctx)
      | None ->     
        failwith 
        (*Util.Error.fail_error *)
          ("Error: the name " ^ Names.string_of_name n
         ^ " is not defined in the environment "
          (*^ Type_ctx.string_of_var_ctx type_ctx.var_ctx *)
          ^ " .")
      end
  | PatUnit -> (TUnit, type_ctx)
  | PatInt _ -> (TInt, type_ctx)
  | PatBool _ -> (TBool, type_ctx)
  | PatRecord labelled_pats -> 
    let labelled_typs = List.fold_left 
     (fun l (lab, pat) -> let (ty, _) =  infer_ptype type_ctx pat in 
      (lab, ty)::l) [] labelled_pats in 
      (TRecord labelled_typs, type_ctx) 
  | PatVariant (lab, pat) -> 
    let (vty, type_ctx') = infer_ptype type_ctx pat in 
    (TVariant [(lab, vty)], type_ctx')
      
and infer_ctype type_ctx = function 
  | Return value -> 
    let (vty, type_ctx') = infer_vtype type_ctx value in 
    (TComp (vty, effpure), type_ctx') 
  | Let (var, comp1, comp2) ->
    let (TComp (vty, _), type_ctx') = infer_ctype type_ctx comp1 in 
    let type_ctx'' = Types.extend_var_ctx type_ctx' var vty in 
    infer_ctype type_ctx'' comp2
  | Match (value, match_cases) ->
    let (match_vty, type_ctx') = infer_vtype type_ctx value in
    let ret_vty = TComp (fresh_typevar(), effpure) in
    let aux_unif (mty, cty, _) (pat, mcase) = 
      let (pty, type_ctx'') = infer_ptype type_ctx' pat in 
      match unify_type type_ctx''.type_subst (mty, pty) with 
       | Some (_, type_subst) -> begin
         let (cty', type_ctx''') = 
           infer_ctype (Types.update_type_subst type_ctx'' type_subst) mcase in 
         match unify_ctype type_ctx'''.type_subst (cty, cty') with 
           | Some (rty, type_subst') -> 
             (mty, rty, Types.update_type_subst type_ctx' type_subst')
           | None -> failwith "Typing error"
           end
       | None -> failwith "Type checking error" in 
    let (_, ret_vty, ty_ctx) = 
    List.fold_left aux_unif (match_vty, ret_vty, type_ctx') match_cases in 
    (ret_vty, ty_ctx)
  | App (val1, val2) as comp-> 
    let (vty2, type_ctx') = infer_vtype type_ctx val2 in
    let (vty1, type_ctx'') = infer_vtype type_ctx' val1 in 
    let vty2' = apply_vtype_subst vty2 type_ctx''.type_subst in
      (*Util.Debug.print_debug ("Typing App:" ^ Syntax.string_of_term expr);
      Util.Debug.print_debug
        (Syntax.string_of_term e1 ^ " is of type " ^ string_of_typ fty);
      Util.Debug.print_debug
        (Syntax.string_of_term e2 ^ " is of type " ^ string_of_typ aty'
       ^ " (was " ^ string_of_typ aty ^ ")"); *)
      begin
        match vty1 with
        | TVar a ->
            let tvar = fresh_typevar () in
            let new_type_a = TArrow (vty2', TComp (tvar, effpure)) in
            let type_ctx''' =
              Types.extend_type_subst type_ctx'' a new_type_a in
            (TComp (tvar, effpure), type_ctx''')
        | TArrow (ty', rty) -> begin
            match unify_type type_ctx''.type_subst (ty', vty2') with
            | Some (_, type_subst) ->
                let type_ctx''' =
                  Types.update_type_subst type_ctx'' type_subst in
                (rty, type_ctx''')
            | None ->
                failwith
                  ("Error typing "
                  ^ Syntax.string_of_computation comp
                  ^ ": " ^ string_of_vtyp ty' ^ " is not equal to "
                  ^ string_of_vtyp vty2')
          end
        | _ ->
            Util.Error.fail_error
              ("Error typing "
              ^ Syntax.string_of_computation comp
              ^ ": " ^ string_of_vtyp vty1 ^ " is not an arrow type")
      end 
  | Handle (comp, h) ->
    let (cty, type_ctx') = infer_ctype type_ctx comp in
    (match cty with TComp (vty, _) -> 
        let (var, comp') = h.ret in
        let type_ctx'' = extend_var_ctx type_ctx' var vty in 
      infer_ctype type_ctx'' comp')     
  | Perform (opsym, _) -> 
     (* not type checking the value here*)
    match ConsCtx.find_opt opsym type_ctx.cons_ctx with 
      | Some (Arity (_, vty)) -> (TComp (vty, effpure), type_ctx)
      | Some _ -> failwith (string_of_opsymbol opsym ^ " is not a constructor") 
      | None -> failwith (string_of_opsymbol opsym ^ " is not defined") 

and check_vtype type_ctx value res_vty =
  let (vty, type_ctx') = infer_vtype type_ctx value in
  match vty with
  | _ when vty = res_vty -> (vty, type_ctx')
  | TVar a -> (res_vty, extend_type_subst type_ctx' a res_vty)
  | _ ->
      Util.Error.fail_error
        ("Error typing "
        ^ string_of_value value
        ^ " : " ^ string_of_vtyp vty ^ " is not equal to "
        ^ string_of_vtyp res_vty)

and check_type type_ctx term cty =
  let (cty', type_ctx') = infer_ctype type_ctx term in
  match cty' with
  | _ when cty = cty' -> (cty', type_ctx')
  | TComp (TVar a, _) -> begin 
      match cty with 
       | TComp (vty, _) -> (cty, extend_type_subst type_ctx' a vty)
    end
  | _ ->
      Util.Error.fail_error
        ("Error typing "
        ^ string_of_computation term
        ^ " : " ^ string_of_ctyp cty' ^ " is not equal to "
        ^ string_of_ctyp cty)

and check_type_bin type_ctx com_vty v1 v2 res_vty =
  let (vty1, type_ctx') = check_vtype type_ctx v1 com_vty in
  let (vty2, type_ctx'') = check_vtype type_ctx' v2 com_vty in
  Util.Debug.print_debug
    ("Typing binary operators "
    ^ string_of_value v1 ^ " and " ^ string_of_value v2 ^ " with " 
    ^ string_of_vtyp com_vty ^ ":" ^ string_of_vtyp vty1 ^ " and  "
    ^ string_of_vtyp vty2);
  (res_vty, type_ctx'')

(*let infer_gen_type type_ctx expr =
  let (ty, type_ctx) = infer_type type_ctx expr in
  let tvar_l = get_tvars ty in
  (TForall (tvar_l, ty), type_ctx)
*)

let typing_full type_subst term =
  let names = Syntax.get_names_comp term in
  let name_ctx = NameSet.fold 
   (fun n nctx -> NameCtx.add n (fresh_typevar()) nctx)
    names NameCtx.empty in
  let type_ctx =
    {var_ctx= VarCtx.empty;
     name_ctx;
     cons_ctx = ConsCtx.empty;
     type_subst} in
  let (cty, _) = infer_ctype type_ctx term in cty
