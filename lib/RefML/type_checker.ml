open Syntax
open Types

exception TypingError of string


let rec infer_type vctx lctx nctx tsubst expr = match expr with
  | Var x ->
    begin match Util.Pmap.lookup x vctx with
      | Some ty -> (ty,vctx,tsubst)
      | None -> Util.Error.fail_error ("Error: the variable " ^ (Syntax.string_of_id x) ^ " is not defined in the environment "
          ^ (string_of_var_ctx vctx) ^
          " .")
    end
  | Name n ->
    begin match Util.Pmap.lookup n nctx with
    | Some ty -> (ty,vctx,tsubst)
    | None -> Util.Error.fail_error ("Error: the name " ^ (Syntax.string_of_name n) ^ " is not defined in the environment "
        ^ (string_of_name_ctx nctx) ^
        " .")
  end
  | Loc l ->
    begin match Util.Pmap.lookup l lctx with
      | Some ty -> (ty,vctx,tsubst)
      | None -> Util.Error.fail_error ("Error: the location " ^ (Syntax.string_of_loc l) ^ " is not defined.")
    end
  | Named (cn,e) ->
    begin match Util.Pmap.lookup cn nctx with
      | Some (TNeg ty) -> check_type vctx lctx nctx tsubst e ty
      | Some ty -> Util.Error.fail_error ("Error: the continuation name " ^ (Syntax.string_of_name cn) ^ " is of type "
                                     ^ string_of_typeML ty ^".")      
      | None -> Util.Error.fail_error ("Error: the continuation name " ^ (Syntax.string_of_name cn) ^ " is not defined.")
    end
  | Unit -> (TUnit,vctx,tsubst)
  | Int _ -> (TInt,vctx,tsubst)
  | Bool _ -> (TBool,vctx,tsubst)
  | BinaryOp(Plus,e1,e2) | BinaryOp(Minus,e1,e2) | BinaryOp(Mult,e1,e2) | BinaryOp(Div,e1,e2) -> begin
      try check_type_bin vctx lctx nctx tsubst TInt e1 e2 TInt
      with TypingError msg -> Util.Error.fail_error ("Error typing Arithmetic Operator " ^ (Syntax.string_of_exprML expr) ^ ": " ^ msg) end
  | BinaryOp(And,e1,e2) | BinaryOp(Or,e1,e2) -> check_type_bin vctx lctx nctx tsubst TBool e1 e2 TBool
  | BinaryOp(Equal,e1,e2) | BinaryOp(NEqual,e1,e2) | BinaryOp(Less,e1,e2) | BinaryOp(LessEq,e1,e2) | BinaryOp(Great,e1,e2) | BinaryOp(GreatEq,e1,e2)  -> 
    check_type_bin vctx lctx nctx tsubst TInt e1 e2 TBool
  | UnaryOp(Not,e) -> check_type vctx lctx nctx tsubst e TBool
  | If (e1,e2,e3) ->
    let (_,vctx1,tsubst1) = check_type vctx lctx nctx tsubst e1 TBool in
    let (ty2,vctx2,tsubst2) = infer_type vctx1 lctx nctx tsubst1 e2 in
    let (ty3,vctx3,tsubst3) = infer_type vctx2 lctx nctx tsubst2 e3 in
    let ty2' = apply_type_subst ty2 tsubst3 in
    Util.Debug.print_debug ("Typing of the first branch:" ^ (string_of_typeML ty2') ^ " (was " ^ (string_of_typeML ty2) ^ ")");
    Util.Debug.print_debug ("Typing of the second branch:" ^ (string_of_typeML ty3));
    begin match unify_type tsubst3 (ty2',ty3) with
      | Some (ty,tsubst4) -> 
        Util.Debug.print_debug ("Typing If:" ^ (string_of_typeML ty)); 
        (ty,lsubst_vctx tsubst4 vctx3,tsubst4)
      | None -> Util.Error.fail_error ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                                  ^(string_of_typeML ty2) ^ " is not equal to "
                                  ^(string_of_typeML ty3))
    end
  | Fun ((var,TUndef),e) ->
    let tvar = fresh_typevar () in
    let (ty',vctx',tsubst') = infer_type (Util.Pmap.modadd_pmap (var,tvar) vctx) lctx nctx tsubst  e in
    begin match Util.Pmap.lookup var vctx' with
      | Some ty -> (TArrow (ty,ty'),vctx',tsubst')
      | None -> failwith ("Variable " ^ (Syntax.string_of_id var) ^ " not found in type-checking. Please report.")
    end
  | Fun ((var,ty),e) ->
    let (ty',vctx',tsubst') = infer_type (Util.Pmap.modadd_pmap (var,ty) vctx) lctx nctx tsubst e in
    (TArrow (ty,ty'),vctx',tsubst')
  | Fix ((idfun,TUndef),(var,TUndef),e) ->
    let tvar1 = fresh_typevar () in
    let tvar2 = fresh_typevar () in
    let new_vctx = Util.Pmap.modadd_pmap2 (var,tvar1) (idfun,TArrow (tvar1,tvar2)) vctx in
    let (rty,vctx',tsubst') = infer_type new_vctx lctx nctx tsubst e in
    begin match (Util.Pmap.lookup var vctx',Util.Pmap.lookup idfun vctx') with
      | (Some aty, Some fty) ->
        begin match unify_type tsubst' (fty,TArrow (aty,rty)) with
          | Some (ty,tsubst'') -> (ty,lsubst_vctx tsubst'' vctx',tsubst'')
          | None -> Util.Error.fail_error ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                                      ^(string_of_typeML fty) ^ " is not equal to "
                                      ^(string_of_typeML (TArrow (aty,rty))))
        end
      | _ -> failwith "Variables not found in type-checking. Please report."
    end
  | Fix ((idfun,TUndef),(var,aty),e) ->
    let tvar2 = fresh_typevar () in
    let new_vctx = Util.Pmap.modadd_pmap2 (var,aty) (idfun,TArrow (aty,tvar2)) vctx in
    let (rty,vctx',tsubst') = infer_type new_vctx lctx nctx tsubst e in
    begin match Util.Pmap.lookup idfun vctx' with
      | Some fty -> 
        begin match unify_type tsubst' (fty,TArrow (aty,rty)) with
          | Some (ty,tsubst'') -> (ty,lsubst_vctx tsubst'' vctx',tsubst'')
          | None -> Util.Error.fail_error ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                                      ^(string_of_typeML fty) ^ " is not equal to "
                                      ^(string_of_typeML (TArrow (aty,rty))))
        end
      | None -> failwith ("Variable " ^ (Syntax.string_of_id idfun) ^ " not found type-checking. Please report.")
    end
  | Fix ((idfun,fty),(var,aty),e) ->
    let new_vctx = Util.Pmap.modadd_pmap2 (var,aty) (idfun,fty) vctx in
    let (rty,vctx',tsubst') =  infer_type new_vctx lctx nctx tsubst e in
    begin match unify_type tsubst' (fty,TArrow (aty,rty)) with
      | Some (ty,tsubst'') -> (ty,lsubst_vctx tsubst'' vctx',tsubst'')
      | None -> Util.Error.fail_error ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                                  ^(string_of_typeML fty) ^ " is not equal to "
                                  ^(string_of_typeML (TArrow (aty,rty))))
    end
  | Let (var,e1,e2) -> 
    let (ty,vctx',tsubst') = infer_type vctx lctx nctx tsubst e1 in
    let new_vctx = Util.Pmap.modadd_pmap (var,ty) vctx' in
    infer_type new_vctx lctx nctx tsubst' e2
  | LetPair (var1,var2,e1,e2) ->
    let (ty,vctx',tsubst') = infer_type vctx lctx nctx tsubst e1 in
    begin match ty with
      | TProd (ty1,ty2) ->
        let new_vctx = Util.Pmap.modadd_pmap2 (var1,ty1) (var2,ty2) vctx' in
        infer_type new_vctx lctx nctx tsubst' e2
      | TVar tvar ->
        let tvar1 = fresh_typevar () in
        let tvar2 = fresh_typevar () in
        let vctx'' = subst_vctx tvar (TProd (tvar1,tvar2)) vctx' in
        let new_vctx = Util.Pmap.modadd_pmap2 (var1,tvar1) (var2,tvar2) vctx'' in
        let new_tsubst = Util.Pmap.modadd_pmap (tvar,TProd (tvar1,tvar2)) tsubst' in
        infer_type new_vctx lctx nctx new_tsubst e2
      | _ -> Util.Error.fail_error ("Error typing " ^ (Syntax.string_of_exprML expr)
                               ^ " : " ^ (string_of_typeML ty) ^ " is not a product type")
    end
  | App (e1,e2) ->
    let (aty,vctx',tsubst') = infer_type vctx lctx nctx tsubst e2 in
    let (fty,vctx'',tsubst'') = infer_type vctx' lctx nctx tsubst' e1 in
    let aty' = apply_type_subst aty tsubst'' in
    Util.Debug.print_debug ("Typing App:" ^ (Syntax.string_of_exprML expr));
    Util.Debug.print_debug ((Syntax.string_of_exprML e1) ^ " is of type " ^ (string_of_typeML fty));
    Util.Debug.print_debug ((Syntax.string_of_exprML e2) ^ " is of type " ^ (string_of_typeML aty') ^ " (was " ^ (string_of_typeML aty) ^")");
    begin match fty with
      | TVar a ->
        let tvar = fresh_typevar () in
        let new_type_a = TArrow (aty',tvar) in
        (tvar,subst_vctx a new_type_a vctx'',Util.Pmap.modadd_pmap (a,new_type_a) tsubst'')
      | TArrow (ty',rty) ->
        begin match unify_type tsubst'' (ty',aty') with
          | Some (_,tsubst''') -> (rty,lsubst_vctx tsubst''' vctx'',tsubst''')
          | None -> Util.Error.fail_error ("Error typing " ^ (Syntax.string_of_exprML expr) ^ ": "
                                      ^ (string_of_typeML ty') ^ " is not equal to "
                                      ^(string_of_typeML aty'))
        end
      | _ -> Util.Error.fail_error ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                               ^ (string_of_typeML fty) ^ " is not an arrow type")
    end
  | Seq (e1,e2) -> 
    let (_,vctx',tsubst') = check_type vctx lctx nctx tsubst e1 TUnit in 
    infer_type vctx' lctx nctx tsubst' e2
  | While (e1,e2) ->
    let (_,vctx',tsubst') = check_type vctx lctx nctx tsubst e1 TBool in
    check_type vctx' lctx nctx tsubst' e2 TUnit     
  | Pair (e1,e2) -> 
    let (ty1,vctx',tsubst') = infer_type vctx lctx nctx tsubst e1 in
    let (ty2,vctx'',tsubst'') = infer_type vctx' lctx nctx tsubst' e2 in (TProd (ty1,ty2),vctx'',tsubst'')
  | Newref (_,e) -> 
    let (ty ,vctx',tsubst')= infer_type vctx lctx nctx tsubst e in
    begin match ty with
    | TVar a -> (TRef TInt,subst_vctx a TInt vctx',Util.Pmap.modadd_pmap (a,TInt) tsubst')
    | TInt -> (TRef ty,vctx',tsubst')
    | _ -> Util.Error.fail_error ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                               ^ (string_of_typeML ty) ^ " is not of type int. Only integers can be stored in references.")
  end
  | Deref e ->
    let (ty,vctx',tsubst') = infer_type vctx lctx nctx tsubst e in
    begin match ty with
      | TRef ty -> (ty,vctx',tsubst')
      | _ -> Util.Error.fail_error ("Error typing " ^ (Syntax.string_of_exprML expr) ^ " : "
                               ^ (string_of_typeML ty) ^ " is not a ref type")
    end
  | Assign (e1,e2) ->
    let (ty1,vctx',tsubst') = infer_type vctx lctx nctx tsubst e1 in
    let (ty2,vctx'',tsubst'') = infer_type vctx' lctx nctx tsubst' e2 in
    begin match (ty1,ty2) with
      | (TRef ty1',_) when ty1' = ty2 -> (TUnit,vctx'',tsubst'')
      | (TRef ty1',TVar a) -> (TUnit,subst_vctx a ty1' vctx'',tsubst'')
      | (TRef (TVar a),_) -> (TUnit,subst_vctx a ty2 vctx'',tsubst'')
      | (_,_) -> Util.Error.fail_error ("Error typing " ^ (Syntax.string_of_exprML expr)
                                   ^ " : " ^ (string_of_typeML ty1) ^ " is not a ref type")
    end
  | Assert e ->
    let (ty,vctx',tsubst') = infer_type vctx lctx nctx tsubst e in
    begin match ty with
      | TBool -> (TUnit, vctx', tsubst')
      | _ -> Util.Error.fail_error ("Error typing " ^ (Syntax.string_of_exprML expr)
        ^ " : " ^ (string_of_typeML ty) ^ " is not equal to bool")
    end
  | Hole -> failwith ("Error: The typechecker cannot type a hole")
  | ECtx _ -> failwith ("Error: The typechecker cannot type an evaluation context")

and check_type vctx lctx nctx tsubst expr res_ty =
  let (ty ,vctx',tsubst')= infer_type vctx lctx nctx tsubst expr in
  match ty with
  | TVar a -> (res_ty,subst_vctx a res_ty vctx',Util.Pmap.modadd_pmap (a,res_ty) tsubst')
  | _ when ty = res_ty -> (ty,vctx',tsubst')
  | _ -> Util.Error.fail_error ("Error typing " ^ (Syntax.string_of_exprML expr) 
                           ^ " : " ^ ((string_of_typeML ty) ^ " is not equal to " 
                                      ^(string_of_typeML res_ty)))

and check_type_bin vctx lctx nctx tsubst com_ty expr1 expr2 res_ty =
  let (ty1,vctx',tsubst') = infer_type vctx lctx nctx tsubst expr1 in
  let (ty2,vctx'',tsubst'') = infer_type vctx' lctx nctx tsubst' expr2 in
  let ty1' = apply_type_subst ty1 tsubst'' in
  Util.Debug.print_debug ("Typing binary operators " ^ (Syntax.string_of_exprML expr1) 
                     ^ " and " ^ (Syntax.string_of_exprML expr2) ^ " with " ^ (string_of_typeML com_ty)  
                     ^ ":" ^ (string_of_typeML ty1') ^ " and  " ^ (string_of_typeML ty2) );
  match (ty1',ty2) with
  | (TVar a1,TVar a2) -> 
    let vctx_new = subst_vctx a1 com_ty vctx'' in
    let vctx_new' = subst_vctx a2 com_ty vctx_new in
    (res_ty, vctx_new',Util.Pmap.modadd_pmap2 (a1,com_ty) (a2,com_ty) tsubst'')
  | (TVar a1, _) when ty2 = com_ty -> 
    let vctx_new = subst_vctx a1 com_ty vctx'' in
    (res_ty,vctx_new,Util.Pmap.modadd_pmap (a1,com_ty) tsubst'')
  | (_,TVar a2) when ty1 = com_ty -> 
    let vctx_new = subst_vctx a2 com_ty vctx'' in
    (res_ty,vctx_new,Util.Pmap.modadd_pmap (a2,com_ty) tsubst'')
  | (_,_) when (ty1' = com_ty) && (ty2 = com_ty) -> (res_ty,vctx'',tsubst'')
  | (_,_) when (ty1' <> com_ty) -> raise (TypingError ((string_of_typeML ty1) ^ " is not equal to " ^(string_of_typeML com_ty)))
  | (_,_) -> raise (TypingError ((string_of_typeML ty2) ^ " is not equal to " ^(string_of_typeML com_ty)))

let typing_full tsubst expr =
  let lnames = Syntax.get_names expr in
  let name_ctx = Util.Pmap.list_to_pmap @@ List.map (fun n -> let tvar = fresh_typevar () in (n,tvar)) lnames in
  let (ty,_,_) = infer_type Util.Pmap.empty Util.Pmap.empty name_ctx tsubst expr in
  ty