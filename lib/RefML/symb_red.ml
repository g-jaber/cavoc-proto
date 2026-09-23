open Syntax
open Logic
open Util.Pmap

type symbconf =
  Syntax.term
  * Syntax.val_env
  * Heap.heap
  * Heap.heap
  * Symbolic.symbolic_ctx
  * Logic.arith_pred list

let add_locvar heap v = Heap.allocate heap v

(* A fresh symbolic variable of the given type, with its declaration. *)
let symbolic_var_of_type = function
  | Types.TUnit -> (Unit, [])
  | (Types.TInt | Types.TBool) as ty ->
      let x = Symbolic.fresh_symbolic () in
      (Symbolic (Symbolic.Kvar x), [ (x, ty) ])
  | ty ->
      failwith
        ("Symbolic evaluation has no symbolic variable of type " ^ Types.string_of_typ ty)

(* The symbolic variable standing for the content of a ground location. *)
let symbolic_var_of_loc loc_ctx l =
  match Util.Pmap.lookup l loc_ctx with
  | Some ty -> symbolic_var_of_type ty
  | None ->
      failwith
        ("The location " ^ string_of_loc l
       ^ " is outside the ground heap context")

let aux g (a, b, c, d, e, f) = (g a, b, c, d, e, f)

let aux_bin_red symbred cons_op = function
  | (expr1, expr2) when isval expr1 ->
      let (result, b) = symbred expr2 in
      (List.map (aux (fun x -> cons_op (expr1, x))) result, b)
  | (expr1, expr2) ->
      let (result, b) = symbred expr1 in
      (List.map (aux (fun x -> cons_op (x, expr2))) result, b)

let aux_bin_arith iop consfun expr1 expr2 heapPost symbred =
  match (expr1, expr2) with
  | (Int n1, Int n2) ->
      let n = iop n1 n2 in
      ( [
          ( Int n,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            [],
            [] );
        ],
        true )
  | (Int _, Symbolic _) | (Symbolic _, Int _) | (Symbolic _, Symbolic _) ->
      let (newvar, decl) = symbolic_var_of_type Types.TInt in
      ( [
          ( newvar,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            decl,
            [ AEqual (newvar, consfun (expr1, expr2)) ] );
        ],
        true )
  | (expr1, expr2) -> aux_bin_red symbred consfun (expr1, expr2)

let aux_bin_arithbool iop consfun expr1 expr2 heapPost symbred =
  match (expr1, expr2) with
  | (Int n1, Int n2) ->
      let b = iop n1 n2 in
      ( [
          ( Bool b,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            [],
            [] );
        ],
        true )
  | (Int _, Symbolic _) | (Symbolic _, Int _) | (Symbolic _, Symbolic _) ->
      ( [
          ( Bool true,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            [],
            [ expr_to_arith_pred (consfun (expr1, expr2)) ] );
          ( Bool false,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            [],
            [ negate_arith_pred (expr_to_arith_pred (consfun (expr1, expr2))) ]
          );
        ],
        true )
  | (expr1, expr2) -> aux_bin_red symbred consfun (expr1, expr2)

(* A Boolean symbolic variable splits on the constraint that it holds. *)
let split_bool heapPost guard expr_true expr_false =
  let holds = AEqual (guard, Bool true) in
  ( [
      (expr_true, Util.Pmap.empty, Util.Pmap.empty, heapPost, [], [ holds ]);
      ( expr_false,
        Util.Pmap.empty,
        Util.Pmap.empty,
        heapPost,
        [],
        [ negate_arith_pred holds ] );
    ],
    true )

let aux_bin_bool iop consfun expr1 expr2 heapPost symbred =
  match (expr1, expr2) with
  | (Bool b1, Bool b2) ->
      let b = iop b1 b2 in
      ( [
          ( Bool b,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            [],
            [] );
        ],
        true )
  | (Symbolic _, _) ->
      split_bool heapPost expr1
        (consfun (Bool true, expr2))
        (consfun (Bool false, expr2))
  | _ -> aux_bin_red symbred consfun (expr1, expr2)

let rec symbred loc_ctx heapPost expr =
  match expr with
  | expr when isval expr ->
      ([ (expr, Util.Pmap.empty, Util.Pmap.empty, heapPost, [], []) ], false)
  | App (Fun ((var, _), expr1), expr2) when isval expr2 ->
      ( [
          ( subst_var expr1 var expr2,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            [],
            [] );
        ],
        true )
  | App ((Fix ((idfun, _), (var, _), expr1) as fix), expr2) when isval expr2 ->
      ( [
          ( subst_var (subst_var expr1 var expr2) idfun fix,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            [],
            [] );
        ],
        true )
  | App (Name _, expr2) when isval expr2 ->
      ([ (expr, Util.Pmap.empty, Util.Pmap.empty, heapPost, [], []) ], false)
  | App (expr1, expr2) ->
      aux_bin_red (symbred loc_ctx heapPost)
        (fun (x, y) -> App (x, y))
        (expr1, expr2)
  | Seq (Unit, expr2) ->
      ( [
          ( expr2,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            [],
            [] );
        ],
        true )
  | Seq (expr1, expr2) ->
      let (result, b) = symbred loc_ctx heapPost expr1 in
      (List.map (aux (fun x -> Seq (x, expr2))) result, b)
  | Pair (expr1, expr2) ->
      aux_bin_red (symbred loc_ctx heapPost)
        (fun (x, y) -> Pair (x, y))
        (expr1, expr2)
  | Let (var, expr1, expr2) when isval expr1 ->
      ( [
          ( subst_var expr2 var expr1,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            [],
            [] );
        ],
        true )
  | Let (var, expr1, expr2) ->
      let (result, b) = symbred loc_ctx heapPost expr1 in
      (List.map (aux (fun x -> Let (var, x, expr2))) result, b)
  | LetPair (var1, var2, Pair (expr1, expr2), expr')
    when isval expr1 && isval expr2 ->
      let expr'' = subst_var expr' var1 expr1 in
      let expr'' = subst_var expr'' var2 expr2 in
      ( [
          ( expr'',
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            [],
            [] );
        ],
        true )
  | LetPair (var1, var2, expr1, expr2) ->
      let (result, b) = symbred loc_ctx heapPost expr1 in
      (List.map (aux (fun x -> LetPair (var1, var2, x, expr2))) result, b)
  | Newref (ty, expr) ->
      if isval expr then
        let (l, heapPost') = add_locvar heapPost expr in
        ([ (Loc l, Util.Pmap.empty, Util.Pmap.empty, heapPost', [], []) ], true)
      else
        let (result, b) = symbred loc_ctx heapPost expr in
        (List.map (aux (fun x -> Newref (ty, x))) result, b)
  | Deref (Loc l) -> begin
      match lookup l heapPost with
      | Some value ->
          ( [
              ( value,
                Util.Pmap.empty,
                Util.Pmap.empty,
                heapPost,
                [],
                [] );
            ],
            true )
      | None ->
          let (x, decl) = symbolic_var_of_loc loc_ctx l in
          let heapPre = Util.Pmap.singleton (l, x) in
          ( [
              ( x,
                Util.Pmap.empty,
                heapPre,
                Util.Pmap.concat heapPre heapPost,
                decl,
                [] );
            ],
            true )
    end
  | Deref expr ->
      let (result, b) = symbred loc_ctx heapPost expr in
      (List.map (aux (fun x -> Deref x)) result, b)
  | Assign (Loc l, expr2) when isval expr2 -> begin
      match lookup l heapPost with
      | Some _ ->
          ( [
              ( Unit,
                Util.Pmap.empty,
                Util.Pmap.empty,
                modadd (l, expr2) heapPost,
                [],
                [] );
            ],
            true )
      | None ->
          let (x, decl) = symbolic_var_of_loc loc_ctx l in
          let heapPre = Util.Pmap.singleton (l, x) in
          ( [
              ( Unit,
                Util.Pmap.empty,
                heapPre,
                modadd (l, expr2) heapPost,
                decl,
                [] );
            ],
            true )
    end
  | Assign (expr1, expr2) ->
      aux_bin_red (symbred loc_ctx heapPost)
        (fun (x, y) -> Assign (x, y))
        (expr1, expr2)
  | If (Bool b, expr1, expr2) ->
      if b then
        ( [
            ( expr1,
              Util.Pmap.empty,
              Util.Pmap.empty,
              heapPost,
              [],
              [] );
          ],
          true )
      else
        ( [
            ( expr2,
              Util.Pmap.empty,
              Util.Pmap.empty,
              heapPost,
              [],
              [] );
          ],
          true )
  | If ((Symbolic _ as guard), expr1, expr2) ->
      split_bool heapPost guard expr1 expr2
  | If (expr, expr1, expr2) ->
      let (result, b) = symbred loc_ctx heapPost expr in
      (List.map (aux (fun x -> If (x, expr1, expr2))) result, b)
  | BinaryOp (Div, expr1, Int 0) when isval expr1 ->
      ([ (Error, Util.Pmap.empty, Util.Pmap.empty, heapPost, [], []) ], true)
  | BinaryOp (Div, expr1, (Symbolic _ as expr2)) when isval expr1 ->
      let zero = AEqual (expr2, Int 0) in
      let consfun = Syntax.get_consfun_from_bin_cons expr in
      let (result, _) =
        aux_bin_arith ( / ) consfun expr1 expr2 heapPost
          (symbred loc_ctx heapPost) in
      let nonzero (expr', gamma, heapPre, heapPost, vars, preds) =
        (expr', gamma, heapPre, heapPost, vars, negate_arith_pred zero :: preds)
      in
      ( (Error, Util.Pmap.empty, Util.Pmap.empty, heapPost, [], [ zero ])
        :: List.map nonzero result,
        true )
  | BinaryOp ((Plus as op), expr1, expr2)
  | BinaryOp ((Minus as op), expr1, expr2)
  | BinaryOp ((Mult as op), expr1, expr2)
  | BinaryOp ((Div as op), expr1, expr2) ->
      let iop = Syntax.implement_arith_op op in
      let consfun = Syntax.get_consfun_from_bin_cons expr in
      aux_bin_arith iop consfun expr1 expr2 heapPost (symbred loc_ctx heapPost)
  | BinaryOp (And, Bool false, _) ->
      ([ (Bool false, Util.Pmap.empty, Util.Pmap.empty, heapPost, [], []) ], true)
  | BinaryOp (Or, Bool true, _) ->
      ([ (Bool true, Util.Pmap.empty, Util.Pmap.empty, heapPost, [], []) ], true)
  | BinaryOp (And, Bool true, expr2) | BinaryOp (Or, Bool false, expr2) ->
      ([ (expr2, Util.Pmap.empty, Util.Pmap.empty, heapPost, [], []) ], true)
  | BinaryOp ((And as op), expr1, expr2) | BinaryOp ((Or as op), expr1, expr2)
    ->
      let iop = Syntax.implement_bin_bool_op op in
      let consfun = Syntax.get_consfun_from_bin_cons expr in
      aux_bin_bool iop consfun expr1 expr2 heapPost (symbred loc_ctx heapPost)
  | UnaryOp (Not, Bool b) ->
      ( [
          ( Bool (not b),
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            [],
            [] );
        ],
        true )
  | UnaryOp (Not, (Symbolic _ as b)) ->
      split_bool heapPost b (Bool false) (Bool true)
  | UnaryOp (Not, expr) ->
      let (result, b) = symbred loc_ctx heapPost expr in
      (List.map (aux (fun x -> UnaryOp (Not, x))) result, b)
  | BinaryOp ((Equal as op), expr1, expr2)
  | BinaryOp ((NEqual as op), expr1, expr2)
  | BinaryOp ((Less as op), expr1, expr2)
  | BinaryOp ((LessEq as op), expr1, expr2)
  | BinaryOp ((Great as op), expr1, expr2)
  | BinaryOp ((GreatEq as op), expr1, expr2) ->
      let iop = Syntax.implement_compar_op op in
      let consfun = Syntax.get_consfun_from_bin_cons expr in
      aux_bin_arithbool iop consfun expr1 expr2 heapPost
        (symbred loc_ctx heapPost)
  | While (guard, body) ->
      ( [
          ( If (guard, Seq (body, While (guard, body)), Unit),
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            [],
            [] );
        ],
        true )
  | Assert (Bool b) ->
      ( [
          ( (if b then Unit else Error),
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            [],
            [] );
        ],
        true )
  | Assert (Symbolic _ as guard) -> split_bool heapPost guard Unit Error
  | Assert expr ->
      let (result, b) = symbred loc_ctx heapPost expr in
      (List.map (aux (fun x -> Assert x)) result, b)
  | Nondet ty ->
      let (x, decl) = symbolic_var_of_type ty in
      ([ (x, Util.Pmap.empty, Util.Pmap.empty, heapPost, decl, []) ], true)
  | Raise expr ->
      let (result, b) = symbred loc_ctx heapPost expr in
      (List.map (aux (fun x -> Raise x)) result, b)
  | Error ->
      ([ (expr, Util.Pmap.empty, Util.Pmap.empty, heapPost, [], []) ], false)
  | Match (expr, handler_l) when isval expr ->
      (select_handler heapPost expr Error handler_l, true)
  | Match (expr, handler_l) ->
      let (result, b) = symbred loc_ctx heapPost expr in
      (List.map (aux (fun x -> Match (x, handler_l))) result, b)
  | TryWith (expr, handler_l) -> begin
      let (result, b) = symbred loc_ctx heapPost expr in
      if b then (List.map (aux (fun x -> TryWith (x, handler_l))) result, true)
      else
        match get_nf_term expr with
        | Nf.NFValue ((), value) ->
            ( [ (value, Util.Pmap.empty, Util.Pmap.empty, heapPost, [], []) ],
              true )
        | Nf.NFRaise ((), value) ->
            (select_handler heapPost value (Raise value) handler_l, true)
        | _ ->
            ( [ (expr, Util.Pmap.empty, Util.Pmap.empty, heapPost, [], []) ],
              false )
    end
  | Constructor (cons, Some expr) ->
      let (result, b) = symbred loc_ctx heapPost expr in
      (List.map (aux (fun x -> Constructor (cons, Some x))) result, b)
  | Record fields ->
      let (id, field) =
        List.find
          (fun (_, field) -> not (isval field))
          (Util.Pmap.to_list fields) in
      let (result, b) = symbred loc_ctx heapPost field in
      (List.map (aux (fun x -> Record (modadd (id, x) fields))) result, b)
  | Projection (Record fields, id) when isval (Record fields) ->
      ( [
          ( lookup_exn id fields,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            [],
            [] );
        ],
        true )
  | Projection (expr, id) ->
      let (result, b) = symbred loc_ctx heapPost expr in
      (List.map (aux (fun x -> Projection (x, id))) result, b)
  | _ ->
      failwith
        ("Error: " ^ string_of_term expr
       ^ " is outside of the fragment of the symbolic evaluation.")

(* The outcomes of matching a value against a pattern: the constraints of each
   one and, when it matches, its substitution. A literal pattern on a symbolic variable
   splits as a comparison does. *)
and match_outcomes pattern value =
  match (pattern, value) with
  | (PatInt n, Symbolic _) ->
      [
        ([ AEqual (value, Int n) ], Some []); ([ ANEqual (value, Int n) ], None);
      ]
  | (PatBool b, Symbolic _) ->
      let holds = AEqual (value, Bool b) in
      [ ([ holds ], Some []); ([ negate_arith_pred holds ], None) ]
  | (PatPair (pattern1, pattern2), Pair (value1, value2)) ->
      List.concat_map
        (function
          | (preds1, None) -> [ (preds1, None) ]
          | (preds1, Some substitution1) ->
              List.map
                (fun (preds2, outcome2) ->
                  ( preds2 @ preds1,
                    Option.map (fun s2 -> substitution1 @ s2) outcome2 ))
                (match_outcomes pattern2 value2))
        (match_outcomes pattern1 value1)
  | _ -> [ ([], match_pattern_with_value pattern value) ]

(* The handlers are tried in order; on_none is the term reached when none
   matches. *)
and select_handler heapPost value on_none = function
  | [] -> [ (on_none, Util.Pmap.empty, Util.Pmap.empty, heapPost, [], []) ]
  | Handler (pattern, expr_branch) :: rest ->
      let substitute e (id, v) = subst_var e id v in
      List.concat_map
        (function
          | (preds, Some substitution) ->
              [
                ( List.fold_left substitute expr_branch substitution,
                  Util.Pmap.empty,
                  Util.Pmap.empty,
                  heapPost,
                  [],
                  preds );
              ]
          | (preds, None) ->
              List.map
                (fun (expr', gamma, heapPre, heapPost, vars, preds') ->
                  (expr', gamma, heapPre, heapPost, vars, preds' @ preds))
                (select_handler heapPost value on_none rest))
        (match_outcomes pattern value)

type run_result =
  | Normal_form of symbconf
  | Divergence of symbconf
  | Incomplete of symbconf

let symbred_trans ?(bound = 1000) ~check_sat loc_ctx conf =
  let rec symbred_trans bound seen
      ((expr, gamma, heapPre, heapPost, vars, preds) as conf) =
    let aux (expr', gamma', heapPre', heapPost', vars', preds') =
      ( expr',
        Util.Pmap.concat gamma' gamma,
        Util.Pmap.concat heapPre' heapPre,
        heapPost',
        vars' @ vars,
        preds' @ preds ) in
    let feasible (_, _, _, _, vars', preds') =
      preds' = [] || check_sat vars' preds' <> Arith_solver.Unsat in
    if bound = 0 then [ Incomplete conf ]
    else if List.mem (expr, heapPost) seen then [ Divergence conf ]
    else
      let (result, b) = symbred loc_ctx heapPost expr in
      if not b then List.map (fun conf' -> Normal_form (aux conf')) result
      else
        List.flatten
          (List.map
             (symbred_trans (bound - 1) ((expr, heapPost) :: seen))
             (List.filter feasible (List.map aux result))) in
  symbred_trans bound [] conf

let compute_nf ?bound ~check_sat loc_ctx heapPost expr =
  symbred_trans ?bound ~check_sat loc_ctx
    (expr, Util.Pmap.empty, Util.Pmap.empty, heapPost, [], [])
