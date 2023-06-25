open Syntax

type op_conf = Syntax.exprML*Syntax.functional_env*Heap.heap

let rec red (expr,env,heap) = match expr with
  | App (Fun ((var,_),expr1),expr2) when (isval expr2) ->
    let expr1' = subst expr1 (Var var) expr2 in
    ((expr1', Pmap.empty, heap),true)
  | App (Fix ((idfun,_),(var,ty),expr1),expr2) when (isval expr2) ->
    let expr' = subst expr1 (Var var) expr2 in
    let env' = Pmap.add (idfun,Fun ((var,ty),expr1)) env in
    ((expr',env',heap),true)
  | App _ -> 
    failwith "Evaluation of terms not in ANF is not yet implemented."
  | Seq (Unit,expr2) -> 
    ((expr2,Pmap.empty,heap),true)
  | Seq (expr1,expr2) -> 
      let ((expr1',env',heap'),isred) = red (expr1,env,heap) in
      ((Seq (expr1',expr2),env',heap'),isred)
  | Pair _ -> failwith "Evaluation of terms not in ANF is not yet implemented."
  | Let (var,expr1,expr2) when (isval expr1) ->
    let expr' = subst expr2 (Var var) expr1 in
    ((expr', env, heap),true)
  | Let (var,expr1,expr2) -> 
    let ((expr1',env',heap'),isred) = red (expr1,env,heap) in
    ((Let (var,expr1',expr2),env',heap'),isred)
  | LetPair (var1,var2,Pair (expr1,expr2),expr) ->
    begin match (isval expr1,isval expr2) with
      | (true,true) ->
        let expr' = subst expr (Var var1) expr1 in
        let expr'' = subst expr' (Var var2) expr2 in
        ((expr'', Pmap.empty, heap),true)
      | (true,false) ->
        let ((expr2',env',heap'),isred) = red (expr2,env,heap) in
        ((LetPair (var1,var2,expr1,expr2'),env',heap'),isred)
      | (false,_) ->
        let ((expr1',env',heap'),isred) = red (expr1,env,heap) in
        ((LetPair (var1,var2,expr1',expr2),env',heap'),isred)
    end
  | Newref expr -> 
    if (isval expr) then
      let (l,heap') = Heap.allocate heap expr in 
      ((Loc l,Pmap.empty,heap'),true)
    else 
      let ((expr',env',heap'),isred) = red (expr,env,heap) in
      ((Newref expr',env',heap'),isred)
  | Deref (Loc l) -> 
    begin match Heap.access heap l with
      | Some value -> ((value,Pmap.empty,heap),true)
      | None -> failwith "Small footprint  not yet implemented"
    end
  | Deref expr ->
    let ((expr',env',heap'),isred) = red (expr,env,heap) in
    ((Deref expr',env',heap'),isred)
  | Assign (Loc l,expr) when (isval expr) ->
    begin match Heap.access heap l with
      | Some _ ->  ((Unit,Pmap.empty,Heap.modify heap l expr),true)
      | None -> failwith "Small footprint approach not yet implemented"
    end
  | Assign _  -> failwith "Evaluation of terms not in ANF is not yet implemented."
  | If (Bool b,expr1,expr2) -> 
      if b then 
        ((expr1,Pmap.empty,heap),true)
      else ((expr2,Pmap.empty,heap),true)
  | If (expr,expr1,expr2) -> 
    let ((expr',env',heap'),isred) = red (expr,env,heap) in
    ((If (expr',expr1,expr2),env',heap'),isred)
  | Plus _ | Minus _ | Mult _ | Div _ -> 
    let (expr1,expr2,arith_op,_) = Syntax.get_aop_from_expr expr in 
    begin match (expr1, expr2) with
    | (Int n1, Int n2) -> 
      let n = arith_op n1 n2 in 
      ((Int n,Pmap.empty,heap),true)
    | _ -> failwith "Evaluation of terms not in ANF is not yet implemented."
    end
  | And _ | Or _ -> 
    let (expr1,expr2,bool_op,_) = Syntax.get_bop_from_expr expr in 
    begin match (expr1, expr2) with
    | (Bool b1, Bool b2) ->
      let b = bool_op b1 b2 in
      ((Bool b,Pmap.empty,heap),true)
    | _ -> failwith "Evaluation of terms not in ANF is not yet implemented."
    end
  | Not (Bool b) -> 
      ((Bool (not b),Pmap.empty,heap),true)
  | Not expr ->
    let ((expr',env',heap'),isred) = red (expr,env,heap) in
    ((Not expr',env',heap'),isred)
  | Equal _ | NEqual _ | Less _ | LessEq _ | Great _ | GreatEq _ ->
    let (expr1,expr2,arithbool_op,_) = Syntax.get_abop_from_expr expr in 
    begin match (expr1, expr2) with
    | (Int n1, Int n2) ->
      let b = arithbool_op n1 n2 in
      ((Bool b,Pmap.empty,heap),true)
    | _ -> failwith "Evaluation of terms not in ANF is not yet implemented."
    end
  | _ -> ((expr,Pmap.empty,heap),false)

let rec compute_nf op_conf =
  let (op_conf',isred) = red op_conf in
  if isred then compute_nf op_conf'
  else op_conf