open Syntax

type store = Store.store
type opconf = Syntax.term * store

let string_of_opconf (expr, store) =
  "(" ^ string_of_term expr ^ " | " ^ Store.string_of_store store ^ ")"

include Util.Monad.BranchState (struct type t = opconf list end)

let empty_state = []

let check_cycle ((expr, _) as opconf) =
  match expr with
  | While _ | Fix _ ->
      let* opconf_list = get () in
      return (List.mem opconf opconf_list)
  | _ -> return false

let add ((expr, _) as opconf) =
  match expr with
  | While _ | Fix _ ->
      let* opconf_list = get () in
      set (opconf :: opconf_list)
  | _ -> return ()

let interpreter interpreter (expr, store) =
  Util.Debug.print_debug ("Interpreter on : " ^ Syntax.string_of_term expr);
  match expr with
  | value when isval value -> return (value, store)
  | Var var -> begin
      match Store.var_lookup store var with
      | None -> return (expr, store)
      | Some value -> return (value, store)
    end
  | App (expr1, expr2) ->
      let* (expr1', store') = interpreter (expr1, store) in
      begin
        match expr1' with
        | Fun ((var, _), body) ->
            let* (expr2', store'') = interpreter (expr2, store') in
            if isval expr2' then
              let body' = subst_var body var expr2' in
              interpreter (body', store'')
            else return (App (expr1', expr2'), store'')
        | Fix ((fvar, _), (var, _), body) ->
            let* (expr2', store'') = interpreter (expr2, store') in
            if isval expr2' then
              let body' = subst_var body var expr2' in
              let body'' = subst_var body' fvar expr1 in
              interpreter (body'', store'')
            else return (App (expr1', expr2'), store'')
        | Name _ ->
            let* (expr2', store'') = interpreter (expr2, store') in
            return (App (expr1', expr2'), store'')
        | _ -> return (App (expr1', expr2), store')
      end
  | Seq (expr1, expr2) ->
      let* (expr1', store') = interpreter (expr1, store) in
      begin
        match expr1' with
        | Unit -> interpreter (expr2, store')
        | _ -> return (Seq (expr1', expr2), store')
      end
  | While (guard, body) ->
      let* (guard', store') = interpreter (guard, store) in
      begin
        match guard' with
        | Bool true ->
            let* (body', store'') = interpreter (body, store') in
            begin
              match body' with
              | Unit -> interpreter (expr, store'')
              | _ -> return (Seq (body', While (guard, body)), store'')
            end
        | Bool false -> return (Unit, store')
        | _ ->
            Util.Debug.print_debug "Callback inside a guard !";
            return (While (guard', body), store')
      end
  | Pair (expr1, expr2) ->
      let* (value1, store1) = interpreter (expr1, store) in
      let* (value2, store2) = interpreter (expr2, store1) in
      return (Pair (value1, value2), store2)
  | Let (var, expr1, expr2) ->
      let* (expr1', store') = interpreter (expr1, store) in
      if isval expr1' then
        let expr' = subst_var expr2 var expr1' in
        interpreter (expr', store')
      else return (Let (var, expr1', expr2), store')
  | LetPair (var1, var2, expr1, expr2) ->
      let* (nf1, store') = interpreter (expr1, store) in
      begin
        match nf1 with
        | Pair (value1, value2) ->
            let expr2' = subst_var expr2 var1 value1 in
            let expr2'' = subst_var expr2' var2 value2 in
            interpreter (expr2'', store')
        | _ -> return (LetPair (var1, var2, nf1, expr2), store')
      end
  | Newref (ty, expr) ->
      let* (nf, store') = interpreter (expr, store) in
      if isval nf then
        let (l, store'') = Store.loc_allocate store' nf in
        return (Loc l, store'')
      else return (Newref (ty, nf), store')
  | Deref expr ->
      let* (nf, store') = interpreter (expr, store) in
      begin
        match nf with
        | Loc l -> begin
            match Store.loc_lookup store' l with
            | Some value -> return (value, store')
            | None ->
                failwith
                  ("Error in the interpreter: " ^ Syntax.string_of_loc l
                 ^ " is not in the store "
                  ^ Store.string_of_store store)
          end
        | _ -> return (Deref nf, store')
      end
  | Assign (expr1, expr2) ->
      let* (nf1, store') = interpreter (expr1, store) in
      begin
        match nf1 with
        | Loc l ->
            let* (nf2, store'') = interpreter (expr2, store') in
            if isval nf2 then
              let store''' = Store.loc_modify store'' l nf2 in
              return (Unit, store''')
            else return (Assign (nf1, nf2), store'')
        | _ -> return (Assign (nf1, expr2), store')
      end
  | If (guard, expr1, expr2) ->
      let* (nf_guard, store') = interpreter (guard, store) in
      begin
        match nf_guard with
        | Bool true -> interpreter (expr1, store')
        | Bool false -> interpreter (expr2, store')
        | _ -> return (If (nf_guard, expr1, expr2), store')
      end
  | BinaryOp ((Plus as op), expr1, expr2)
  | BinaryOp ((Minus as op), expr1, expr2)
  | BinaryOp ((Mult as op), expr1, expr2)
  | BinaryOp ((Div as op), expr1, expr2) ->
      let* (nf1, store1) = interpreter (expr1, store) in
      begin
        match nf1 with
        | Int n1 ->
            let* (nf2, store2) = interpreter (expr2, store1) in
            begin
              match nf2 with
              | Int n2 ->
                  let iop = Syntax.implement_arith_op op in
                  let n = iop n1 n2 in
                  return (Int n, store2)
              | _ -> return (BinaryOp (op, nf1, nf2), store2)
            end
        | _ -> return (BinaryOp (op, nf1, expr2), store1)
      end
  | BinaryOp ((And as op), expr1, expr2) | BinaryOp ((Or as op), expr1, expr2)
    ->
      let* (nf1, store1) = interpreter (expr1, store) in
      begin
        match nf1 with
        | Bool b1 ->
            let* (nf2, store2) = interpreter (expr2, store1) in
            begin
              match nf2 with
              | Bool b2 ->
                  let iop = Syntax.implement_bin_bool_op op in
                  let b = iop b1 b2 in
                  return (Bool b, store2)
              | _ -> return (BinaryOp (op, nf1, nf2), store2)
            end
        | _ -> return (BinaryOp (op, nf1, expr2), store1)
      end
  | UnaryOp (Not, expr) ->
      let* (nf, store') = interpreter (expr, store) in
      begin
        match nf with
        | Bool b -> return (Bool (not b), store')
        | _ -> return (UnaryOp (Not, nf), store')
      end
  | BinaryOp ((Equal as op), expr1, expr2)
  | BinaryOp ((NEqual as op), expr1, expr2)
  | BinaryOp ((Less as op), expr1, expr2)
  | BinaryOp ((LessEq as op), expr1, expr2)
  | BinaryOp ((Great as op), expr1, expr2)
  | BinaryOp ((GreatEq as op), expr1, expr2) ->
      let* (nf1, store1) = interpreter (expr1, store) in
      begin
        match nf1 with
        | Int n1 ->
            let* (nf2, store2) = interpreter (expr2, store1) in
            begin
              match nf2 with
              | Int n2 ->
                  let iop = Syntax.implement_compar_op op in
                  let b = iop n1 n2 in
                  return (Bool b, store2)
              | _ -> return (BinaryOp (op, nf1, nf2), store2)
            end
        | _ -> return (BinaryOp (op, nf1, expr2), store1)
      end
  | Assert guard ->
      let* (guard', store') = interpreter (guard, store) in
      begin
        match guard' with
        | Bool false -> return (Error, store')
        | Bool true -> return (Unit, store')
        | _ ->
            Util.Debug.print_debug "Callback inside an assert!";
            return (Assert guard', store')
      end
  | Raise expr ->
      let* (nf, store') = interpreter (expr, store) in
      return (Raise nf, store')
  | TryWith (expr, handler_l) ->
      let* (nf, store') = interpreter (expr, store) in
      if isval nf then return (nf, store')
      else begin
        match nf with
        | Raise (Constructor c as cons) ->
            let rec aux = function
              | Handler (pat, expr_pat) :: handler_l' -> begin
                  match pat with
                  | PatCons c' when c = c' -> interpreter (expr_pat, store')
                  | PatCons _ -> aux handler_l'
                  | PatVar id ->
                      let expr' = subst_var expr_pat id cons in
                      interpreter (expr', store')
                end
              | [] -> return (Raise cons, store') in
            aux handler_l
        | _ -> return (TryWith (nf, handler_l), store')
      end
  | _ ->
      failwith
        ("Error: " ^ Syntax.string_of_term expr
       ^ " is outside of the fragment we consider.")

let normalize_opconf_monad opconf =
  let rec aux opconf =
    let* b = check_cycle opconf in
    if b then begin
      Util.Debug.print_debug
        ("The operational configuration " ^ string_of_opconf opconf
       ^ " is diverging");
      fail ()
    end
    else
      let* _ = add opconf in
      interpreter aux opconf in
  aux opconf

let normalize_opconf opconf =
  let comp = normalize_opconf_monad opconf in
  let (res, _) = runState comp empty_state in
  match res with
  | [] -> None
  | [ nf ] -> Some nf
  | _ -> failwith "Error: non-determinism in the evaluation. Please report"

let normalize_term_env comp_list =
  let rec aux store = function
    | [] -> store
    | (var, comp) :: comp_list' ->
        if isval comp then
          let store' = Store.var_add store (var, comp) in
          aux store' comp_list'
        else begin
          match normalize_opconf (comp, store) with
          | None ->
              failwith @@ "The operational configuration "
              ^ string_of_opconf (comp, store)
              ^ " is diverging."
          | Some (value, store') ->
              let store'' = Store.var_add store' (var, value) in
              aux store'' comp_list'
        end in
  aux Store.empty_store comp_list
