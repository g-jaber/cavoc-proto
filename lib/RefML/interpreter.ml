open Syntax

type store = Store.store
type opconf = Syntax.term * store

let string_of_opconf (expr, store) =
  "(" ^ string_of_term expr ^ " | " ^ Store.string_of_store store ^ ")"

(**
  The monad used by the interpreter. It is a branching monad with state
  local to each branch.
 *)
module SymbolicEvalState = struct
    (* The state monad already emulates lazy evaluation of branches *)
    type 'a m =  store -> ('a * store) list

    let return (x : 'a) : 'a m =
      fun store -> [ x, store ]

    let ( let* ) (m : 'a m) (f : 'a -> 'b m) : 'b m =
      fun store ->
        let xs = m store in
        List.concat @@ List.map (fun (x, store) -> f x store) xs

    let lookup id : value m =
      fun store ->
        match Store.var_lookup store id with
        | Some v -> [ v, store ]
        | None -> failwith ("Unbound variable: " ^ id)

    let alloc expr : loc m =
      fun store -> [ Store.loc_allocate store expr ]

    let get loc : value m =
      fun store ->
        match Store.loc_lookup store loc with
        | Some v -> [ v, store ]
        | None -> failwith (string_of_loc loc ^ " is not in the store "
                    ^ Store.string_of_store store)

    let set loc expr : unit m =
      fun store -> [ (), Store.loc_modify store loc expr ]

    let store () =
      fun store -> [ store, store ]

    let para_pair ma mb =
      fun store -> ma store @ mb store

    let run m store =
      m store

    let fail () =
      fun _ -> []

    (**
      select computation t and/or f depending on the satisfiability of
      k/not k.
     *)
    let branch k t f : 'a m =
      let set_store store = fun _ -> [ (), store ] in

      let branch_with m b =
        let k = if b then k else Symbolic.neg k in
        let* store = store () in
        let  store = Store.symbolic_add_constraint store k in

        Util.Debug.print_debug
          (Printf.sprintf "branch: checking for sat of %s..."
            (Symbolic.string_of_constraint k)) ;

        (* TODO: maybe checking for UNSAT would be a better idea.
                 If k is UNSAT, we know which branch to take. Also,
                 adding further constraints cannot make k SAT, which
                 could enable further optimizations? *)
        if Symbolic.check_sat store.symbolic_ctx then
          let* _ = set_store store in m
        else
          fail ()
      in

      para_pair (branch_with t true) (branch_with f false)
end

open SymbolicEvalState

let interpreter interpreter expr : value SymbolicEvalState.m =
  Util.Debug.print_debug ("Interpreter on: " ^ Syntax.string_of_term expr);
  match expr with
  | value when isval value -> return value
  | Var var -> lookup var
  | Constructor (cons, Some expr) -> 
      let* expr = interpreter expr in
      return @@ Constructor (cons, Some expr)
  | Constructor (_cons, None) -> failwith "Empty constructor not implemented yet (interpreter)"
  | App (expr1, expr2) ->
      let* expr1 = interpreter expr1 in
      begin
        match expr1 with
        | Fun ((var, _), body) ->
            let* expr2 = interpreter expr2 in
            if isval expr2 then
              let body = subst_var body var expr2 in
              interpreter body
            else
              return @@ App (expr1, expr2)
        | Fix ((fvar, _), (var, _), body) ->
            let* expr2 = interpreter expr2 in
            if isval expr2 then
              let body = subst_var body var expr2 in
              let body = subst_var body fvar expr1 in
              interpreter body
            else
              return @@ App (expr1, expr2)
        | Name _ ->
            let* expr2 = interpreter expr2 in
            return @@ App (expr1, expr2)
        | _ -> return @@ App (expr1, expr2)
      end
  | Seq (expr1, expr2) ->
      let* expr1 = interpreter expr1 in
      if expr1 = Unit then
        interpreter expr2
      else
        return @@ Seq (expr1, expr2)
  | While (guard, body) ->
      let* guard = interpreter guard in
      begin
        let loop () =
          let* body = interpreter body in
          if body = Unit then
            interpreter expr
          else
            return @@ Seq (body, While (guard, body))
        in
        match guard with
        | Symbolic k1 -> branch k1 (loop ()) (return Unit)
        | Bool true -> loop ()
        | Bool false -> return Unit
        | _ ->
            Util.Debug.print_debug "Callback inside a guard !" ;
            return @@ While (guard, body)
      end
  | Pair (expr1, expr2) ->
      let* value1 = interpreter expr1 in
      let* value2 = interpreter expr2 in
      return @@ Pair (value1, value2)
  | Let (var, expr1, expr2) ->
      let* expr1 = interpreter expr1 in
      if isval expr1 then
        let expr = subst_var expr2 var expr1 in
        interpreter expr
      else
        return @@ Let (var, expr1, expr2)
  | LetPair (var1, var2, expr1, expr2) ->
      let* nf1 = interpreter expr1 in
      begin
        match nf1 with
        | Pair (value1, value2) ->
            let expr2 = subst_var expr2 var1 value1 in
            let expr2 = subst_var expr2 var2 value2 in
            interpreter expr2
        | _ -> return @@ LetPair (var1, var2, nf1, expr2)
      end
  | Newref (ty, expr) ->
      let* nf = interpreter expr in
      if isval nf then
        let* l = alloc nf in
        return @@ Loc l
      else
        return @@ Newref (ty, nf)
  | Deref expr ->
      let* nf = interpreter expr in
      begin
        match nf with
        | Loc l -> get l
        | _ -> return @@ Deref nf
      end
  | Assign (expr1, expr2) ->
      let* nf1 = interpreter expr1 in
      begin
        match nf1 with
        | Loc l ->
            let* nf2 = interpreter expr2 in
            if isval nf2 then
              let* _ = set l nf2 in
              return Unit
            else
              return @@ Assign (nf1, nf2)
        | _ -> return @@ Assign (nf1, expr2)
      end
  | If (guard, expr1, expr2) ->
      let* guard = interpreter guard in
      begin
        match guard with
        | Symbolic k1 -> branch k1 (interpreter expr1) (interpreter expr2)
        | Bool b -> if b then interpreter expr1 else interpreter expr2
        | _ -> return @@ If (guard, expr1, expr2)
      end
  | BinaryOp ((Plus as op), expr1, expr2)
  | BinaryOp ((Minus as op), expr1, expr2)
  | BinaryOp ((Mult as op), expr1, expr2)
  | BinaryOp ((Div as op), expr1, expr2) ->
      let* nf1 = interpreter expr1 in
      begin
        match nf1 with
        | Int n1 ->
            let* nf2 = interpreter expr2 in
            begin
              match nf2 with
              | Int n2 ->
                  let iop = Syntax.implement_arith_op op in
                  let n = iop n1 n2 in
                  return @@ (Int n)
              | _ -> return @@ BinaryOp (op, nf1, nf2)
            end
        | _ -> return @@ BinaryOp (op, nf1, expr2)
      end
  | BinaryOp ((And as op), expr1, expr2) | BinaryOp ((Or as op), expr1, expr2)
    ->
      let* nf1 = interpreter expr1 in
      begin
        match nf1, op with
        | (Bool true, Or) | (Bool false, And) ->
            return @@ Bool (op = Or) (* short circuit *)
        | (Bool b1, _) ->
            let* nf2 = interpreter expr2 in
            begin
              match nf2 with
              | Bool b2 ->
                  return @@ Bool (Syntax.implement_bin_bool_op op b1 b2)
              | Symbolic k2 ->
                  return @@ Symbolic k2
              | _ -> return @@ BinaryOp (op, nf1, nf2)
            end
        | (Symbolic k1, _) ->
            let* nf2 = interpreter expr2 in
            begin
              match nf2, op with
              | (Bool true, Or) | (Bool false, And) ->
                  return @@ Bool (op = Or)
              | (Bool _, _) ->
                  return @@ Symbolic k1
              | (Symbolic k2, _) ->
                  let open Symbolic in
                  let k3 = if op = And then Kand (k1, k2) else Kor (k1, k2) in
                  return @@ Symbolic k3
              | _ -> return @@ BinaryOp (op, nf1, nf2)
            end
        | _ -> return @@ BinaryOp (op, nf1, expr2)
      end
  | UnaryOp (Not, expr) ->
      let* nf = interpreter expr in
      begin
        match nf with
        | Symbolic k1 ->
            return @@ Symbolic (Symbolic.neg k1)
        | Bool b -> return @@ Bool (not b)
        | _ -> return @@ UnaryOp (Not, nf)
      end
  | BinaryOp ((Equal as op), expr1, expr2)
  | BinaryOp ((NEqual as op), expr1, expr2)
  | BinaryOp ((Less as op), expr1, expr2)
  | BinaryOp ((LessEq as op), expr1, expr2)
  | BinaryOp ((Great as op), expr1, expr2)
  | BinaryOp ((GreatEq as op), expr1, expr2) ->
      let* nf1 = interpreter expr1 in
      begin
        match nf1 with
        | Int n1 ->
            let* nf2 = interpreter expr2 in
            begin
              match nf2 with
              | Int n2 ->
                  let iop = Syntax.implement_compar_op op in
                  let b = iop n1 n2 in
                  return @@ (Bool b)
              | _ -> return @@ BinaryOp (op, nf1, nf2)
            end
        | _ -> return @@ BinaryOp (op, nf1, expr2)
      end
  | Assert guard ->
      let* guard = interpreter guard in
      begin
        match guard with
        | Symbolic k1 -> branch k1 (return Unit) (return Error)
        | Bool false -> return Error
        | Bool true -> return Unit
        | _ ->
            Util.Debug.print_debug "Callback inside an assert!" ;
            return @@ Assert guard
      end
  | Raise expr ->
      let* nf = interpreter expr in
      return @@ Raise nf
  | TryWith (expr, handler_l) ->
      let* nf = interpreter expr in
      if isval nf then
        return nf
      else begin
        match nf with
        | Raise (Constructor (c, Some nf) as cons) ->
            let rec aux = function
              | Handler (pat, expr_pat) :: rest ->
                  begin
                    match pat with
                    | PatCons (c', Some id) when c = c' ->
                        interpreter (subst_var expr_pat id nf)
                    | PatCons (_, None) -> failwith "empty PatCons not implemented yet"
                    | PatCons _ ->
                        aux rest
                    | PatVar id ->
                        interpreter (subst_var expr_pat id cons)
                  end
              | [] -> return @@ Raise cons
            in
            aux handler_l
        | Raise (Constructor (_, None)) -> failwith "Empty constructor not implemented yet (interpreter, TryWith)"
        | _ -> return @@ TryWith (nf, handler_l)
      end
  | Record fields -> (
    let reconstruct_fields m (id, expr) =
      let* new_fields = m in
      let* expr' = interpreter expr in
      return @@ Util.Pmap.add (id, expr') new_fields
    in
    let* reconstructed_fields = 
      Util.Pmap.fold reconstruct_fields (return Util.Pmap.empty) fields in
    return @@ Record reconstructed_fields
  )
  | Projection (expr, id) -> (
    let* nv = interpreter expr in
    match nv with
    | Record fields -> return @@ Util.Pmap.lookup_exn id fields
    | _ -> return @@ Projection (nv, id)
  )
  | _ ->
      failwith
        ("Error: " ^ Syntax.string_of_term expr
        ^ " is outside of the fragment we consider.")

let normalize_opconf_fix expr =
  (* Keep track of seen opconfs to detect program divergence.
     This code never escapes this function so it doesn't have
     to be monadic.
     TODO: Should this be shared between all branches? *)
  let seen = ref [] in

  let check_cycle (expr, _ as opconf) =
    match expr with
    | While _ | App (Fix _, _) -> List.mem opconf !seen
    | _ -> false
  in
  let add (expr, _ as opconf) =
    match expr with
    | While _ | Fix _ -> seen := opconf :: !seen
    | _ -> ()
  in

  let rec aux expr =
    let* store = store () in
    let opconf = expr, store in
    (* this is needed to retrieve the current opconf since the store
       is hidden inside the monad *)
    if check_cycle opconf then begin
      Util.Debug.print_debug
        ("The operational configuration " ^ string_of_opconf opconf
        ^ " is diverging") ;
      fail ()
    end else begin
      add opconf ;
      interpreter aux expr
    end
  in
  aux expr

let normalize_opconf (value, store) =
  run (normalize_opconf_fix value) store

let normalize_term_env cons_ctx comp_list =
  let rec aux store = function
    | [] -> store
    | (var, comp) :: comp_list' ->
        if isval comp then
          let store' = Store.var_add store (var, comp) in
          aux store' comp_list'
        else begin
          match normalize_opconf (comp, store) with
          | [] -> (* We should replace this failwith with a proper error*)
              failwith @@ "The operational configuration "
              ^ string_of_opconf (comp, store)
              ^ " is diverging."
          | (value, store') :: [] ->
              let store'' = Store.var_add store' (var, value) in
              aux store'' comp_list'
          | _ ->
              failwith "Error: Non determinism in the initial evaluation. Please report."
        end in
  let store = Store.embed_cons_ctx cons_ctx in
  aux store comp_list
