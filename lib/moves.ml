open Syntax
open Types

let max_int = 42

let count_fname = ref 0
let fresh_fname () =
  let fn = !count_fname in
  count_fname := !count_fname + 1; ("f" ^ (string_of_int fn))

let count_cname = ref 0
let fresh_cname () =
  let cn = !count_cname in
  count_cname := !count_cname + 1; ("f" ^ (string_of_int cn))

type name_ctx = (id,typeML) Pmap.pmap

let empty_name_ctx = Pmap.empty

type nup = exprML

let rec generate_nup = function
  | TUnit -> [(Unit,empty_name_ctx)]
  | TBool -> [(Bool true,empty_name_ctx); (Bool false,empty_name_ctx)]
  | TInt ->
    let rec aux i =
      if i < 0 then []
      else (Int i,empty_name_ctx)::(aux (i-1))
    in aux max_int
  | TProd (ty1,ty2) ->
    let _ = generate_nup ty1 in
    let _ = generate_nup ty2 in
    failwith "Need to implement the shuffle of lnup1 and lnup2"
  | TSum _ ->
    failwith "Need to add injection to the syntax of expressions"
    (*
    let lnup1 = generate_nup ty1 in
    let lnup1' = List.map (fun (nup,nctx) -> (Inj (1,nup),nctx)) lnup1 in
    let lnup2' = List.map (fun (nup,nctx) -> (Inj (2,nup),nctx)) lnup1 in
    lnup1'@lnup2' *)
  | TArrow _ as ty ->
    let fn = fresh_fname () in
    [(Var fn,Pmap.singleton (fn,ty))]
  | _ -> failwith "error"

let rec abstract_val value ty =
  match (value,ty) with
  | (Fun _,TArrow _) | (Fix _,TArrow _) | (Var _,TArrow _) -> 
    let fn = fresh_fname () in
    let ienv = Pmap.singleton (fn,value) in
    let lnamectx = Pmap.singleton (fn,ty) in
    (Var fn,ienv,lnamectx)
  | (Unit,TUnit) | (Bool _,TBool) | (Int _,TInt) -> (value,Pmap.empty,empty_name_ctx) 
  | (Pair (value1, value2),TProd (ty1,ty2)) ->
    let (nup1,ienv1,lnamectx1) = abstract_val value1 ty1 in
    let (nup2,ienv2,lnamectx2) = abstract_val value2 ty2 in
    (Pair (nup1,nup2),Pmap.concat ienv1 ienv2,Pmap.concat lnamectx1 lnamectx2)
  | _ -> failwith ("Error: "^ (string_of_exprML value) ^ " cannot be abstracted because it is not a value.") 

type interactive_env = (id,exprML) Pmap.pmap

let string_of_interactive_env ienv =
  Pmap.string_of_pmap "ε" Syntax.string_of_exprML "," ienv

type kindTerm =
    IsVal of (id*exprML)
  | IsCallExtern of (id*exprML*eval_context)
  | IsRecCall of (id*exprML*exprML*eval_context)

let decompose_nf (expr,fenc) =
  match extract_ctx expr with 
  | (App ((Var fn), value), ectx) ->
    begin match Pmap.lookup_pmap fn fenc with
      | Some valf -> IsRecCall (fn,valf,value,ectx)
      | _ -> IsCallExtern (fn,value,ectx)
    end
  | (value, Named (cn,Hole)) when isval value ->
      IsVal(cn,value)
  | _ ->
    failwith ("Error: trying to decompose from "
              ^ (string_of_exprML expr))


type action =
  | Op
  | RecCall of id 
  | PQ of id*nup*id
  | PA of id*nup
  | OQ of id*nup*id
  | OA of id*nup