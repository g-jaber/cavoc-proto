open Syntax
open Logic
open Util.Pmap

type symbconf =
  Syntax.exprML
  * Syntax.val_env
  * Logic.symbheap
  * Logic.symbheap
  * Type_ctx.var_ctx
  * Logic.arith_pred list

let add_locvar heap v =
  let l = fresh_locvar () in
  (l, Util.Pmap.add (l, v) heap)

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
            Util.Pmap.empty,
            [] );
        ],
        true )
  | (Int n, Var x) ->
      let newid = fresh_lvar () in
      let newvar = Var newid in
      ( [
          ( newvar,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.singleton (newid, Types.TInt),
            [ AEqual (newvar, consfun (Int n, Var x)) ] );
        ],
        true )
  | (Var x, Int n) ->
      let newid = fresh_lvar () in
      let newvar = Var newid in
      ( [
          ( newvar,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.singleton (newid, Types.TInt),
            [ AEqual (newvar, consfun (Var x, Int n)) ] );
        ],
        true )
  | (Var x1, Var x2) ->
      let newid = fresh_lvar () in
      let newvar = Var newid in
      ( [
          ( newvar,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.singleton (newid, Types.TInt),
            [ AEqual (newvar, consfun (Var x1, Var x2)) ] );
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
            Util.Pmap.empty,
            [] );
        ],
        true )
  | (Int n, Var x) ->
      ( [
          ( Bool true,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.empty,
            [ expr_to_arith_pred (consfun (Int n, Var x)) ] );
          ( Bool false,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.empty,
            [ negate_arith_pred (expr_to_arith_pred (consfun (Int n, Var x))) ]
          );
        ],
        true )
  | (Var x, Int n) ->
      ( [
          ( Bool true,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.empty,
            [ expr_to_arith_pred (consfun (Var x, Int n)) ] );
          ( Bool false,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.empty,
            [ negate_arith_pred (expr_to_arith_pred (consfun (Var x, Int n))) ]
          );
        ],
        true )
  | (Var x1, Var x2) ->
      ( [
          ( Bool true,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.empty,
            [ expr_to_arith_pred (consfun (Var x1, Var x2)) ] );
          ( Bool false,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.empty,
            [
              negate_arith_pred (expr_to_arith_pred (consfun (Var x1, Var x2)));
            ] );
        ],
        true )
  | (expr1, expr2) -> aux_bin_red symbred consfun (expr1, expr2)

let aux_bin_bool iop consfun expr1 expr2 heapPost symbred =
  match (expr1, expr2) with
  | (Bool b1, Bool b2) ->
      let b = iop b1 b2 in
      ( [
          ( Bool b,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.empty,
            [] );
        ],
        true )
  | _ -> aux_bin_red symbred consfun (expr1, expr2)

let rec symbred heapPost expr =
  match expr with
  | App (Fun ((var, _), expr1), expr2) when isval expr2 ->
      ( [
          ( subst_var expr1 var expr2,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.empty,
            [] );
        ],
        true )
  | App (Fix ((idfun, _), (var, ty), expr1), expr2) when isval expr2 ->
      ( [
          ( subst_var expr1 var expr2,
            Util.Pmap.singleton (idfun, Fun ((var, ty), expr1)),
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.empty,
            [] );
        ],
        true )
  | App (expr1, expr2) ->
      aux_bin_red (symbred heapPost) (fun (x, y) -> App (x, y)) (expr1, expr2)
  | Seq (Unit, expr2) ->
      ( [
          ( expr2,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.empty,
            [] );
        ],
        true )
  | Seq (expr1, expr2) ->
      let (result, b) = symbred heapPost expr1 in
      (List.map (aux (fun x -> Seq (x, expr2))) result, b)
  | Pair (expr1, expr2) ->
      aux_bin_red (symbred heapPost) (fun (x, y) -> Pair (x, y)) (expr1, expr2)
  | Let (var, expr1, expr2) when isval expr1 ->
      ( [
          ( subst_var expr2 var expr1,
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.empty,
            [] );
        ],
        true )
  | Let (var, expr1, expr2) ->
      let (result, b) = symbred heapPost expr1 in
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
            Util.Pmap.empty,
            [] );
        ],
        true )
  | LetPair (var1, var2, expr1, expr2) ->
      let (result, b) = symbred heapPost expr1 in
      (List.map (aux (fun x -> LetPair (var1, var2, x, expr2))) result, b)
  | Newref (ty, expr) ->
      if isval expr then
        let (l, heapPost') = add_locvar heapPost expr in
        ( [
            ( Var l,
              Util.Pmap.empty,
              Util.Pmap.empty,
              heapPost',
              Util.Pmap.singleton (l, Types.TRef Types.TInt),
              [] );
          ],
          true ) (* Fix This *)
      else
        let (result, b) = symbred heapPost expr in
        (List.map (aux (fun x -> Newref (ty, x))) result, b)
  | Deref (Var l) -> begin
      match lookup l heapPost with
      | Some value ->
          ( [
              ( value,
                Util.Pmap.empty,
                Util.Pmap.empty,
                heapPost,
                Util.Pmap.empty,
                [] );
            ],
            true )
      | None ->
          let x = Logic.fresh_lvar () in
          let heapPre = Util.Pmap.singleton (l, Var x) in
          ( [
              ( Var x,
                Util.Pmap.empty,
                heapPre,
                Util.Pmap.concat heapPre heapPost,
                Util.Pmap.singleton (x, Types.TInt),
                [] );
            ],
            true )
      (* Fix This *)
    end
  | Deref expr ->
      let (result, b) = symbred heapPost expr in
      (List.map (aux (fun x -> Deref x)) result, b)
  | Assign (Var l, expr2) when isval expr2 -> begin
      match lookup l heapPost with
      | Some _ ->
          ( [
              ( Unit,
                Util.Pmap.empty,
                Util.Pmap.empty,
                modadd (l, expr2) heapPost,
                Util.Pmap.empty,
                [] );
            ],
            true )
      | None ->
          let x = Logic.fresh_lvar () in
          let heapPre = Util.Pmap.singleton (l, Var x) in
          ( [
              ( Unit,
                Util.Pmap.empty,
                heapPre,
                modadd (l, expr2) heapPost,
                Util.Pmap.singleton (x, Types.TInt),
                [] );
            ],
            true )
    end
  | Assign (expr1, expr2) ->
      aux_bin_red (symbred heapPost) (fun (x, y) -> Assign (x, y)) (expr1, expr2)
  | If (Bool b, expr1, expr2) ->
      if b then
        ( [
            ( expr1,
              Util.Pmap.empty,
              Util.Pmap.empty,
              heapPost,
              Util.Pmap.empty,
              [] );
          ],
          true )
      else
        ( [
            ( expr2,
              Util.Pmap.empty,
              Util.Pmap.empty,
              heapPost,
              Util.Pmap.empty,
              [] );
          ],
          true )
  | If (Var _, _, _) ->
      failwith
        "Error: Boolean variables are not allowed in the symbolic reduction. \
         Please report."
  | If (expr, expr1, expr2) ->
      let (result, b) = symbred heapPost expr in
      (List.map (aux (fun x -> If (x, expr1, expr2))) result, b)
  | BinaryOp ((Plus as op), expr1, expr2)
  | BinaryOp ((Minus as op), expr1, expr2)
  | BinaryOp ((Mult as op), expr1, expr2)
  | BinaryOp ((Div as op), expr1, expr2) ->
      let iop = Syntax.implement_arith_op op in
      let consfun = Syntax.get_consfun_from_bin_cons expr in
      aux_bin_arith iop consfun expr1 expr2 heapPost (symbred heapPost)
  | BinaryOp ((And as op), expr1, expr2) | BinaryOp ((Or as op), expr1, expr2)
    ->
      let iop = Syntax.implement_bin_bool_op op in
      let consfun = Syntax.get_consfun_from_bin_cons expr in
      aux_bin_bool iop consfun expr1 expr2 heapPost (symbred heapPost)
  | UnaryOp (Not, Bool b) ->
      ( [
          ( Bool (not b),
            Util.Pmap.empty,
            Util.Pmap.empty,
            heapPost,
            Util.Pmap.empty,
            [] );
        ],
        true )
  (*  | Not (Var b) -> [(Bool true, [],[],heapPost,[AEqual (AExpr(Var b)),(Bool false)]);(Bool false, [], [], heapPost,[AEqual (AExpr(Var b)),(Bool true)])]*)
  | UnaryOp (Not, expr) ->
      let (result, b) = symbred heapPost expr in
      (List.map (aux (fun x -> UnaryOp (Not, x))) result, b)
  | BinaryOp ((Equal as op), expr1, expr2)
  | BinaryOp ((NEqual as op), expr1, expr2)
  | BinaryOp ((Less as op), expr1, expr2)
  | BinaryOp ((LessEq as op), expr1, expr2)
  | BinaryOp ((Great as op), expr1, expr2)
  | BinaryOp ((GreatEq as op), expr1, expr2) ->
      let iop = Syntax.implement_compar_op op in
      let consfun = Syntax.get_consfun_from_bin_cons expr in
      aux_bin_arithbool iop consfun expr1 expr2 heapPost (symbred heapPost)
  | _ ->
      ( [
          (expr, Util.Pmap.empty, Util.Pmap.empty, heapPost, Util.Pmap.empty, []);
        ],
        false )

let rec symbred_trans (expr, gamma, heapPre, heapPost, vars, preds) =
  let aux (expr', gamma', heapPre', heapPost', vars', preds') =
    ( expr',
      Util.Pmap.concat gamma' gamma,
      Util.Pmap.concat heapPre' heapPre,
      heapPost',
      Util.Pmap.concat vars' vars,
      preds' @ preds ) in
  let (result, b) = symbred heapPost expr in
  if not b then List.map aux result
  else
    List.flatten
      (List.map
         (fun (expr', gamma', heapPre', heapPost', vars', preds') ->
           symbred_trans
             ( expr',
               Util.Pmap.concat gamma' gamma,
               Util.Pmap.concat heapPre' heapPre,
               heapPost',
               Util.Pmap.concat vars' vars,
               preds' @ preds ))
         result)

let compute_nf expr =
  symbred_trans
    ( expr,
      Util.Pmap.empty,
      Util.Pmap.empty,
      Util.Pmap.empty,
      Util.Pmap.empty,
      [] )
