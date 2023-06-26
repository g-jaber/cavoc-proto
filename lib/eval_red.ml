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
  | While (Bool false, _) ->
    ((Unit,Pmap.empty,heap),true)
  | While (Bool true, expr1) ->
    ((Seq (expr1,expr),Pmap.empty,heap),true)
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
  | BinaryOp (Plus as op,expr1,expr2) | BinaryOp (Minus as op,expr1,expr2) 
  | BinaryOp (Mult as op,expr1,expr2) | BinaryOp (Div as op,expr1,expr2) ->
    let iop = Syntax.implement_arith_op op in
    begin match (expr1, expr2) with
    | (Int n1, Int n2) -> 
      let n = iop n1 n2 in 
      ((Int n,Pmap.empty,heap),true)
    | _ -> failwith "Evaluation of terms not in ANF is not yet implemented."
    end
    | BinaryOp(And as op,expr1,expr2) | BinaryOp(Or as op,expr1,expr2) ->
      let iop = Syntax.implement_bin_bool_op op in
      begin match (expr1, expr2) with
      | (Bool b1, Bool b2) ->
        let b = iop b1 b2 in
        ((Bool b,Pmap.empty,heap),true)
      | _ -> failwith "Evaluation of terms not in ANF is not yet implemented."
    end
  | UnaryOp(Not,Bool b) -> 
      ((Bool (not b),Pmap.empty,heap),true)
  | UnaryOp(Not,expr) ->
    let ((expr',env',heap'),isred) = red (expr,env,heap) in
    ((UnaryOp(Not,expr'),env',heap'),isred)
  | BinaryOp(Equal as op,expr1,expr2) | BinaryOp(NEqual as op,expr1,expr2) 
  | BinaryOp(Less as op,expr1,expr2) | BinaryOp(LessEq as op,expr1,expr2) 
  | BinaryOp(Great as op,expr1,expr2) | BinaryOp(GreatEq as op,expr1,expr2) ->
    let iop = Syntax.implement_compar_op op in
    begin match (expr1, expr2) with
    | (Int n1, Int n2) ->
      let b = iop n1 n2 in
      ((Bool b,Pmap.empty,heap),true)
    | _ -> failwith "Evaluation of terms not in ANF is not yet implemented."
    end
  | _ -> ((expr,Pmap.empty,heap),false)

let rec compute_nf op_conf =
  let (op_conf',isred) = red op_conf in
  if isred then compute_nf op_conf'
  else op_conf