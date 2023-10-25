open Syntax
open Types

type nup = exprML

let string_of_nup = string_of_exprML

let names_of_nup = get_names

let rec unify_nup nspan nup1 nup2 =
  match (nup1,nup2) with
    | (Unit,Unit) -> Some nspan
    | (Bool b1,Bool b2) -> if b1 = b2 then Some nspan else None
    | (Int n1, Int n2) ->  if n1 = n2 then Some nspan else None
    | (Pair (nup11,nup12), Pair (nup21,nup22)) ->
      let nspan1_option = unify_nup nspan nup11 nup21 in
      begin match nspan1_option with
        | None -> None
        | Some nspan1 -> unify_nup nspan1 nup12 nup22
      end
    | (Name n1, Name n2) -> Util.Namespan.add_nspan (n1,n2) nspan
    | _ -> failwith ("Error: one of the terms " ^ (string_of_nup nup1) ^ " or " ^ (string_of_nup nup2)
      ^ " is not a NUP. Please report.")

let max_int = 2

let rec generate_nup = function
  | TUnit -> [(Unit,Syntax.empty_name_ctx)]
  | TBool -> [(Bool true,Syntax.empty_name_ctx); (Bool false,Syntax.empty_name_ctx)]
  | TInt ->
    let rec aux i =
      if i < 0 then []
      else (Int i,Syntax.empty_name_ctx)::(aux (i-1))
    in aux max_int
  | TProd (ty1,ty2) ->
    let nups1 = generate_nup ty1 in
    let nups2 = generate_nup ty2 in
    let aux (nup1,nctx1) = List.map (fun (nup2,nctx2) -> (Pair (nup1,nup2),Util.Pmap.concat nctx1 nctx2)) nups2
    in List.flatten (List.map aux nups1)
  | TSum _ ->
    failwith "Need to add injection to the syntax of expressions"
    (*
    let lnup1 = generate_nup ty1 in
    let lnup1' = List.map (fun (nup,nctx) -> (Inj (1,nup),nctx)) lnup1 in
    let lnup2' = List.map (fun (nup,nctx) -> (Inj (2,nup),nctx)) lnup1 in
    lnup1'@lnup2' *)
  | TArrow _ as ty ->
    let fn = fresh_fname () in
    [(Name fn, Util.Pmap.singleton (fn,ty))]
  | TNeg _ as ty  -> 
    let cn = fresh_cname () in
    [(Name cn, Util.Pmap.singleton (cn,ty))]
  | TVar _ -> failwith "NUPs for type variables are not yet supported"
  | ty -> failwith ("Error generating a nup on type " ^ (Types.string_of_typeML ty) ^ ". Please report")

type interactive_env = (name,exprML) Util.Pmap.pmap

let empty_ienv = Util.Pmap.empty

let singleton_ienv = Util.Pmap.singleton

let list_to_ienv = Util.Pmap.list_to_pmap

let string_of_interactive_env =
  Util.Pmap.string_of_pmap "ε" " => " Syntax.string_of_name Syntax.string_of_exprML

let lookup_ienv = Util.Pmap.lookup

let concat_ienv = Util.Pmap.concat

let rec abstract_val value ty =
  match (value,ty) with
  | (Fun _,TArrow _) | (Fix _,TArrow _) | (Name _,TArrow _) -> 
    let fn = fresh_fname () in
    let ienv = Util.Pmap.singleton (fn,value) in
    let lnamectx = Util.Pmap.singleton (fn,ty) in
    (Name fn,ienv,lnamectx)
  | (ECtx _,_) ->
    let cn = fresh_cname () in
    let ienv = Util.Pmap.singleton (cn,value) in
    let lnamectx = Util.Pmap.singleton (cn,ty) in
    (Name cn,ienv,lnamectx)
  | (Unit,TUnit) | (Bool _,TBool) | (Int _,TInt) -> (value,Util.Pmap.empty,Syntax.empty_name_ctx) 
  | (Pair (value1, value2),TProd (ty1,ty2)) ->
    let (nup1,ienv1,lnamectx1) = abstract_val value1 ty1 in
    let (nup2,ienv2,lnamectx2) = abstract_val value2 ty2 in
    (Pair (nup1,nup2),Util.Pmap.concat ienv1 ienv2,Util.Pmap.concat lnamectx1 lnamectx2)
  | _ -> failwith ("Error: "
                  ^ (string_of_exprML value) 
                  ^ " of type " 
                  ^ (string_of_typeML ty)
                  ^ " cannot be abstracted because it is not a value.")

type kindTerm =
    Extern of (name*exprML*Heap.heap)
  | IsRecCall of (id*exprML*exprML*eval_context*Heap.heap)
  | Diverge

let decompose_nf (expr,_) =
  match extract_ctx expr with 
    | (App ((Name fn), value), ectx) ->
        (fn,Pair (value,ECtx ectx))
    | (value, Named (cn,Hole)) when isval value ->
        (cn,value)
    | (expr',ectx) ->
      failwith ("Error: the decomposition of the term "
              ^ (string_of_exprML expr)
              ^ " into the redex  " 
              ^ (string_of_exprML expr')
              ^ " and the evaluation context "
              ^ (string_of_exprML ectx)
              ^ " is not in canonical form. Please report.")
let decompose_nf_option = function
    | None -> Diverge
    | Some ((_,heap) as opconf) -> 
      let (nn,value) = decompose_nf opconf in
      Extern (nn,value,heap)
    

let val_composition value nup =
  match (value,nup) with
  | (ECtx ectx,_) -> fill_hole ectx nup
  | (Fun _, Pair (nup',Name cn)) -> Named (cn,App (value,nup'))
  | (Name _, Pair (nup',Name cn)) -> Named (cn,App (value,nup'))
  | _ -> 
    failwith ("Error: the value " 
      ^ (string_of_exprML value)
      ^ " cannot be composed with the NUP "
      ^ (string_of_exprML nup) 
      ^ ". Please report")