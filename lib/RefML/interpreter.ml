open Syntax

type memory = Memory.memory
type opconf = Syntax.term * memory

let string_of_opconf (expr, memory) =
  "(" ^ string_of_term expr ^ " | " ^ Memory.string_of_memory memory ^ ")"

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

let interpreter interpreter (expr, memory) =
  Util.Debug.print_debug ("Interpreter on : " ^ Syntax.string_of_term expr);
  match expr with
  | value when isval value -> return (value, memory)
  | Var var -> begin
      match Memory.var_lookup memory var with
      | None -> return (expr, memory)
      | Some value -> return (value, memory)
    end
  | App (expr1, expr2) ->
      let* (expr1', memory') = interpreter (expr1, memory) in
      begin
        match expr1' with
        | Fun ((var, _), body) ->
            let* (expr2', memory'') = interpreter (expr2, memory') in
            if isval expr2' then
              let body' = subst_var body var expr2' in
              interpreter (body', memory'')
            else return (App (expr1', expr2'), memory'')
        | Fix ((fvar, _), (var, _), body) ->
            let* (expr2', memory'') = interpreter (expr2, memory') in
            if isval expr2' then
              let body' = subst_var body var expr2' in
              let body'' = subst_var body' fvar expr1 in
              interpreter (body'', memory'')
            else return (App (expr1', expr2'), memory'')
        | Name _ ->
            let* (expr2', memory'') = interpreter (expr2, memory') in
            return (App (expr1', expr2'), memory'')
        | _ -> return (App (expr1', expr2), memory')
      end
  | Seq (expr1, expr2) ->
      let* (expr1', memory') = interpreter (expr1, memory) in
      begin
        match expr1' with
        | Unit -> interpreter (expr2, memory')
        | _ -> return (Seq (expr1', expr2), memory')
      end
  | While (guard, body) ->
      let* (guard', memory') = interpreter (guard, memory) in
      begin
        match guard' with
        | Bool true ->
            let* (body', memory'') = interpreter (body, memory') in
            begin
              match body' with
              | Unit -> interpreter (expr, memory'')
              | _ -> return (Seq (body', While (guard, body)), memory'')
            end
        | Bool false -> return (Unit, memory')
        | _ ->
            Util.Debug.print_debug "Callback inside a guard !";
            return (While (guard', body), memory')
      end
  | Pair (expr1, expr2) ->
      let* (value1, memory1) = interpreter (expr1, memory) in
      let* (value2, memory2) = interpreter (expr2, memory1) in
      return (Pair (value1, value2), memory2)
  | Let (var, expr1, expr2) ->
      let* (expr1', memory') = interpreter (expr1, memory) in
      if isval expr1' then
        let expr' = subst_var expr2 var expr1' in
        interpreter (expr', memory')
      else return (Let (var, expr1', expr2), memory')
  | LetPair (var1, var2, expr1, expr2) ->
      let* (nf1, memory') = interpreter (expr1, memory) in
      begin
        match nf1 with
        | Pair (value1, value2) ->
            let expr2' = subst_var expr2 var1 value1 in
            let expr2'' = subst_var expr2' var2 value2 in
            interpreter (expr2'', memory')
        | _ -> return (LetPair (var1, var2, nf1, expr2), memory')
      end
  | Newref (ty, expr) ->
      let* (nf, memory') = interpreter (expr, memory) in
      if isval nf then
        let (l, memory'') = Memory.loc_allocate memory' nf in
        return (Loc l, memory'')
      else return (Newref (ty, nf), memory')
  | Deref expr ->
      let* (nf, memory') = interpreter (expr, memory) in
      begin
        match nf with
        | Loc l -> begin
            match Memory.loc_lookup memory' l with
            | Some value -> return (value, memory')
            | None ->
                failwith
                  ("Error in the interpreter: " ^ Syntax.string_of_loc l
                 ^ " is not in the memory "
                  ^ Memory.string_of_memory memory)
          end
        | _ -> return (Deref nf, memory')
      end
  | Assign (expr1, expr2) ->
      let* (nf1, memory') = interpreter (expr1, memory) in
      begin
        match nf1 with
        | Loc l ->
            let* (nf2, memory'') = interpreter (expr2, memory') in
            if isval nf2 then
              let memory''' = Memory.loc_modify memory'' l nf2 in
              return (Unit, memory''')
            else return (Assign (nf1, nf2), memory'')
        | _ -> return (Assign (nf1, expr2), memory')
      end
  | If (guard, expr1, expr2) ->
      let* (nf_guard, memory') = interpreter (guard, memory) in
      begin
        match nf_guard with
        | Bool true -> interpreter (expr1, memory')
        | Bool false -> interpreter (expr2, memory')
        | _ -> return (If (nf_guard, expr1, expr2), memory')
      end
  | BinaryOp ((Plus as op), expr1, expr2)
  | BinaryOp ((Minus as op), expr1, expr2)
  | BinaryOp ((Mult as op), expr1, expr2)
  | BinaryOp ((Div as op), expr1, expr2) ->
      let* (nf1, memory1) = interpreter (expr1, memory) in
      begin
        match nf1 with
        | Int n1 ->
            let* (nf2, memory2) = interpreter (expr2, memory1) in
            begin
              match nf2 with
              | Int n2 ->
                  let iop = Syntax.implement_arith_op op in
                  let n = iop n1 n2 in
                  return (Int n, memory2)
              | _ -> return (BinaryOp (op, nf1, nf2), memory2)
            end
        | _ -> return (BinaryOp (op, nf1, expr2), memory1)
      end
  | BinaryOp ((And as op), expr1, expr2) | BinaryOp ((Or as op), expr1, expr2)
    ->
      let* (nf1, memory1) = interpreter (expr1, memory) in
      begin
        match nf1 with
        | Bool b1 ->
            let* (nf2, memory2) = interpreter (expr2, memory1) in
            begin
              match nf2 with
              | Bool b2 ->
                  let iop = Syntax.implement_bin_bool_op op in
                  let b = iop b1 b2 in
                  return (Bool b, memory2)
              | _ -> return (BinaryOp (op, nf1, nf2), memory2)
            end
        | _ -> return (BinaryOp (op, nf1, expr2), memory1)
      end
  | UnaryOp (Not, expr) ->
      let* (nf, memory') = interpreter (expr, memory) in
      begin
        match nf with
        | Bool b -> return (Bool (not b), memory')
        | _ -> return (UnaryOp (Not, nf), memory')
      end
  | BinaryOp ((Equal as op), expr1, expr2)
  | BinaryOp ((NEqual as op), expr1, expr2)
  | BinaryOp ((Less as op), expr1, expr2)
  | BinaryOp ((LessEq as op), expr1, expr2)
  | BinaryOp ((Great as op), expr1, expr2)
  | BinaryOp ((GreatEq as op), expr1, expr2) ->
      let* (nf1, memory1) = interpreter (expr1, memory) in
      begin
        match nf1 with
        | Int n1 ->
            let* (nf2, memory2) = interpreter (expr2, memory1) in
            begin
              match nf2 with
              | Int n2 ->
                  let iop = Syntax.implement_compar_op op in
                  let b = iop n1 n2 in
                  return (Bool b, memory2)
              | _ -> return (BinaryOp (op, nf1, nf2), memory2)
            end
        | _ -> return (BinaryOp (op, nf1, expr2), memory1)
      end
  | Assert guard ->
      let* (guard', memory') = interpreter (guard, memory) in
      begin
        match guard' with
        | Bool false -> return (Error, memory')
        | Bool true -> return (Unit, memory')
        | _ ->
            Util.Debug.print_debug "Callback inside an assert!";
            return (Assert guard', memory')
      end
  | Raise expr ->
      let* (nf, memory') = interpreter (expr, memory) in
      return (Raise nf, memory')
  | TryWith (expr, handler_l) ->
      let* (nf, memory') = interpreter (expr, memory) in
      if isval nf then return (nf, memory')
      else begin
        match nf with
        | Raise (Constructor c as cons) ->
            let rec aux = function
              | Handler (pat, expr_pat) :: handler_l' -> begin
                  match pat with
                  | PatCons c' when c = c' -> interpreter (expr_pat, memory')
                  | PatCons _ -> aux handler_l'
                  | PatVar id ->
                      let expr' = subst_var expr_pat id cons in
                      interpreter (expr', memory')
                end
              | [] -> return (Raise cons, memory') in
            aux handler_l
        | _ -> return (TryWith (nf, handler_l), memory')
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
  let rec aux memory = function
    | [] -> memory
    | (var, comp) :: comp_list' ->
        if isval comp then
          let memory' = Memory.var_add memory (var, comp) in
          aux memory' comp_list'
        else begin
          match normalize_opconf (comp, memory) with
          | None ->
              failwith @@ "The operational configuration "
              ^ string_of_opconf (comp, memory)
              ^ " is diverging."
          | Some (value, memory') ->
              let memory'' = Memory.var_add memory' (var, value) in
              aux memory'' comp_list'
        end in
  aux Memory.empty_memory comp_list
