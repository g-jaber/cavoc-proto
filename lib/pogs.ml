open Syntax
open Pmap
open Logic

type arith_ctx = arith_pred list

type id_conf = int

type nup = exprML

let count_fname = ref 0
let fresh_fname () =
  let fn = !count_fname in
  count_fname := !count_fname + 1; ("f" ^ (string_of_int fn))

let count_cname = ref 0
let fresh_cname () =
  let cn = !count_cname in
  count_cname := !count_cname + 1; ("f" ^ (string_of_int cn))

let rec abstract_val value =
  match value with
  | Fun _ | Fix _ | Var _ -> 
    let fn = fresh_fname () in
    let ienv = Pmap.singleton (fn,value) in
    (Var fn,ienv)
  | Unit | Bool _ | Int _ -> (value,Pmap.empty) 
  | Pair (value1, value2) -> 
    let (nup1,ienv1) = abstract_val value1 in
    let (nup2,ienv2) = abstract_val value2 in
    (Pair (nup1,nup2),Pmap.concat ienv1 ienv2)
  | _ -> failwith ("Error: "^ (string_of_exprML value) ^ " cannot be abstracted because it is not a value.") 

type kindTerm =
    IsVal of (id*exprML)
  | IsCallExtern of (id*exprML*eval_context)
  | IsRecCall of (id*exprML*exprML*eval_context)

let decompose_nf (expr,gamma) =
  match extract_ctx expr with 
  | (App ((Var fn), value), ectx) ->
    begin match Pmap.lookup_pmap fn gamma with
      | Some valf -> IsRecCall (fn,valf,value,ectx)
      | _ -> IsCallExtern (fn,value,ectx)
    end
  | (value, Named (cn,Hole)) when isval value ->
      IsVal(cn,value)
  | _ ->
    failwith ("Error: trying to decompose from "
              ^ (string_of_exprML expr))

(*
    let (aval,ienv) = abstract_val value in
    let cn = fresh_cname () in
    let ienv' = Pmap.add (cn,ectx) ienv in
    (fn,aval,ienv')
*)

let count_id_sequent = ref 0

let fresh_id_sequent () =
  let x = !count_id_sequent in
  count_id_sequent := !count_id_sequent + 1;x

type interactive_env = (id,exprML) pmap

type move = id*nup

type active_conf = 
  { id : id_conf;
    ground_var_ctx : var_ctx;
    alpha : (var_ctx,bool) pmap;
    arith_ctx : arith_ctx;
    term : exprML}

type passive_conf = 
  { id : id_conf;
    ground_var_ctx : var_ctx;
    alpha : (var_ctx,bool) pmap;
    arith_ctx : arith_ctx;
    ienv : interactive_env}

(*
type trans_kind =
  | Op
  | RecCall 
  | PQ
  | PA
  | OQ
  | OA

let intern_trans aconf =
  let nf = Symb_red.compute_nf aconf.term in
  match nf with
*)