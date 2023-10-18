open Syntax

type opconf = Syntax.exprML*Heap.heap

let string_of_opconf (expr,heap) = 
  "(" 
  ^ (string_of_exprML expr)
  ^ " | "
  ^ (Heap.string_of_heap heap)
  ^ ")"

include Monad.LStMonad(struct type t = opconf list end)

let empty_state = []

let lookup (expr,heap) = 
  match expr with
   | (While _) -> 
    let* opconf_list = get () in
    return (List.mem (expr,heap) opconf_list)
  | _ -> return false

let add (expr,heap) = 
  match expr with
    | (While _) -> 
      let* opconf_list = get () in
      set ((expr,heap)::opconf_list)
    | _ -> return ()

let interpreter interpreter (expr,heap) = 
  Debug.print_debug ("Interpreter on : " ^ (Syntax.string_of_exprML expr));
  match expr with
  | value when isval value -> return (value,heap) 
  | App (expr1,expr2)  ->
    let* (expr1',heap') = interpreter (expr1,heap) in
    begin match expr1' with
      | Fun ((var,_),body) ->
        let* (expr2',heap'') = interpreter (expr2,heap') in
        if isval expr2' then
          let body' = subst_var body var expr2' in
          interpreter (body', heap'')
        else return (App (expr1',expr2'),heap'')
      | Name _ -> 
        let* (expr2',heap'') = interpreter (expr2,heap') in
        return (App (expr1',expr2'),heap'') 
      | _ -> return (App (expr1',expr2),heap')
    end
  | Seq (expr1,expr2) -> 
    let* (expr1',heap') = interpreter (expr1,heap) in
    begin match expr1' with
      | Unit -> interpreter (expr2,heap')
      | _ -> return (Seq (expr1',expr2),heap')
    end
  | While (guard,body) ->
    let* (guard',heap') = interpreter (guard,heap) in
    begin match guard' with
      | Bool true ->
        let* (body',heap'') = interpreter (body,heap') in
        begin match body' with
          | Unit -> interpreter (expr,heap'')
          | _ -> return (Seq (body',While (guard,body)),heap'')
        end
      | Bool false -> return (Unit,heap')
      | _ ->
        Debug.print_debug "Callback inside a guard !";  
        return (While (guard',body),heap')
    end
  | Pair (expr1,expr2) -> 
    let* (value1,heap1) = interpreter (expr1,heap) in
    let* (value2,heap2) = interpreter (expr2,heap1) in
    return (Pair (value1,value2),heap2)
  | Let (var,expr1,expr2) ->
    let* (expr1',heap') = interpreter (expr1,heap) in
    if isval expr1' then
      let expr' = subst_var expr2 var expr1' in
      interpreter (expr',heap')
    else return (Let (var,expr1',expr2),heap') 
  | LetPair (var1,var2,expr1,expr2) ->
    let* (nf1,heap') = interpreter (expr1,heap) in
    begin match nf1 with
      | Pair (value1,value2) ->
        let expr2' = subst_var expr2 var1 value1 in
        let expr2'' = subst_var expr2' var2 value2 in
        interpreter (expr2'',heap')
      | _ -> return (LetPair (var1,var2,nf1,expr2),heap')
    end
  | Newref (ty,expr) -> 
    let* (nf,heap') = interpreter (expr,heap) in
    if isval nf then 
      let (l,heap'') = Heap.allocate heap' nf in 
        return (Loc l,heap'')
    else return (Newref (ty,nf),heap) 
  | Deref expr -> 
    let* (nf,heap') = interpreter (expr,heap) in
    begin match nf with
      | Loc l -> 
        begin match Heap.access heap l with
          | Some value -> return (value,heap')
          | None -> failwith ("Error in the interpreter: " ^ (Syntax.string_of_loc l) ^ "is not in the heap " ^ (Heap.string_of_heap heap))
        end
      | _ -> return (Deref nf, heap')
    end
  | Assign (expr1,expr2) ->
    let* (nf1,heap') = interpreter (expr1,heap) in
    begin match nf1 with
      | Loc l -> 
        let* (nf2,heap'') = interpreter (expr2,heap') in
        if isval nf2 then return (Unit,Heap.modify heap'' l nf2)
        else return (Assign (nf1,nf2),heap'')
      | _ -> return (Assign (nf1,expr2),heap')
    end
  | If (guard,expr1,expr2) -> 
      let* (nf_guard,heap') = interpreter (guard,heap) in
      begin match nf_guard with
      | Bool true ->
        interpreter (expr1,heap')
      | Bool false ->
        interpreter (expr2,heap')
      | _ -> return (If (nf_guard,expr1,expr2), heap')
    end
  | BinaryOp (Plus as op,expr1,expr2) | BinaryOp (Minus as op,expr1,expr2) 
  | BinaryOp (Mult as op,expr1,expr2) | BinaryOp (Div as op,expr1,expr2) ->
    let* (nf1,heap1) = interpreter (expr1,heap) in
    begin match nf1 with
      | Int n1  ->
        let* (nf2,heap2) = interpreter (expr2,heap1) in
        begin match nf2 with
          | Int n2 -> 
            let iop = Syntax.implement_arith_op op in
            let n = iop n1 n2 in
            return (Int n,heap2)
          | _ -> return (BinaryOp (op,nf1,nf2),heap2)
        end
      | _ -> return (BinaryOp (op,nf1,expr2),heap1)
      end
  | BinaryOp(And as op,expr1,expr2) | BinaryOp(Or as op,expr1,expr2) ->
    let* (nf1,heap1) = interpreter (expr1,heap) in
    begin match nf1 with
      | Bool b1  ->
        let* (nf2,heap2) = interpreter (expr2,heap1) in
        begin match nf2 with
        | Bool b2 -> 
          let iop = Syntax.implement_bin_bool_op op in
          let b = iop b1 b2 in
          return (Bool b,heap2)
        | _ -> return (BinaryOp (op,nf1,nf2),heap2)
        end
      | _ -> return (BinaryOp (op,nf1,expr2),heap1)
      end
  | UnaryOp(Not,expr) -> 
    let* (nf, heap') = interpreter (expr,heap) in
    begin match nf with 
      | Bool b -> return (Bool (not b),heap')
      | _ -> return (UnaryOp(Not,nf),heap')
    end
  | BinaryOp(Equal as op,expr1,expr2) | BinaryOp(NEqual as op,expr1,expr2) 
  | BinaryOp(Less as op,expr1,expr2) | BinaryOp(LessEq as op,expr1,expr2) 
  | BinaryOp(Great as op,expr1,expr2) | BinaryOp(GreatEq as op,expr1,expr2) ->
    let* (nf1,heap1) = interpreter (expr1,heap) in
    begin match nf1 with
      | Int n1  ->
        let* (nf2,heap2) = interpreter (expr2,heap1) in
        begin match nf2 with
          | Int n2 -> 
            let iop = Syntax.implement_compar_op op in
            let b = iop n1 n2 in
            return (Bool b,heap2)
          | _ -> return (BinaryOp (op,nf1,nf2),heap2)
        end
      | _ -> return (BinaryOp (op,nf1,expr2),heap1)
      end
  | Named (cn,expr) -> 
    let* (expr',heap') = interpreter (expr,heap) in
    return (Named (cn,expr'),heap')
  | _ -> failwith ("Error: " ^ (Syntax.string_of_exprML expr) ^ " is outside of the fragment we consider.")

let compute_nf_monad opconf =
  let rec aux opconf =
    let* b = lookup opconf in
    if b then begin 
      Debug.print_debug ("The operational configuration " ^ (string_of_opconf opconf) ^" is diverging") ;
      fail ()
    end
    else let* _ = add opconf in
      interpreter aux opconf
  in aux opconf

let compute_nf opconf =
  let comp = compute_nf_monad opconf in
  let (res,_) = runState comp empty_state in
  match res with
  | [] -> None
  | [nf] -> Some nf
  | _ -> failwith "Error: non-determinism in the evaluation. Please report"