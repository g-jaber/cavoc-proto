open Syntax
open Types

type interactive_val = IVal of Syntax.valML | ICtx of Syntax.eval_context

type glue_val =
  | GVal of Syntax.valML
  | GPair of (Syntax.valML * Syntax.eval_context)

let string_of_interactive_val = function
  | IVal value -> Syntax.string_of_value value
  | ICtx ectx -> Syntax.string_of_eval_context ectx

let embed_val value = IVal value

type interactive_env = (name, interactive_val) Util.Pmap.pmap

let embed_val_env = Util.Pmap.map_im (fun v -> IVal v)

let extract_val_env = Util.Pmap.filter_map (function (n,IVal v) -> Some (n,v) | (_,ICtx _) -> None)

let empty_ienv = Util.Pmap.empty
let singleton_ienv = Util.Pmap.singleton
let list_to_ienv = Util.Pmap.list_to_pmap

let string_of_interactive_env =
  Util.Pmap.string_of_pmap "ε" " => " Syntax.string_of_name
    string_of_interactive_val

let lookup_ienv = Util.Pmap.lookup
let concat_ienv = Util.Pmap.concat

let abstract_glue_val ival ty =
  match (ival, ty) with
  | (GPair (value, ectx), TProd (ty_v, ty_c)) ->
      let (nup, val_env, lnamectx) = Nup.abstract_val value ty_v in
      let cn = fresh_cname () in
      let nup' = Nup.incorporate_name (nup, cn) in
      let ienv = embed_val_env val_env in
      let ienv' = Util.Pmap.add (cn, ICtx ectx) ienv in
      let lnamectx' = Util.Pmap.add (cn, ty_c) lnamectx in
      (nup', ienv', lnamectx')
  | (GVal value, _) ->
      let (nup, val_env, lnamectx) = Nup.abstract_val value ty in
      let ienv =embed_val_env val_env in
      (nup, ienv, lnamectx)
  | (GPair _, _) -> failwith "Ill-typed interactive value. Please report."

type kindTerm =
  | Extern of (name * glue_val * Heap.heap)
  | IsRecCall of (id * exprML * exprML * eval_context * Heap.heap)
  | Diverge

let decompose_nf (expr, _, _) =
  match extract_ctx expr with
  | (App (Name fn, value), ectx) -> (fn, GPair (value, ECtx ectx))
  | (value, Named (cn, Hole)) when isval value -> (cn, GVal value)
  | (expr', ectx) ->
      failwith
        ("Error: the decomposition of the term " ^ string_of_exprML expr
       ^ " into the redex  " ^ string_of_exprML expr'
       ^ " and the evaluation context " ^ string_of_exprML ectx
       ^ " is not in canonical form. Please report.")

let decompose_nf_option = function
  | None -> Diverge
  | Some ((_, _, heap) as opconf) ->
      let (nn, value) = decompose_nf opconf in
      Extern (nn, value, heap)

let val_composition ival nup =
  match (ival, nup) with
  | (ICtx (ECtx ectx), _) -> fill_hole ectx nup
  | (IVal (Fun _ as value), Pair (nup', Name cn)) ->
      Named (cn, App (value, nup'))
  | (IVal (Name _ as value), Pair (nup', Name cn)) ->
      Named (cn, App (value, nup'))
  | _ ->
      failwith
        ("Error: the interactive value "
        ^ string_of_interactive_val ival
        ^ " cannot be composed with the NUP " ^ string_of_exprML nup
        ^ ". Please report")

let subst_names_of_nup ival = Nup.subst_names_of_nup (extract_val_env ival)