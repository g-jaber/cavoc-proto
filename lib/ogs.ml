open Syntax
open Moves

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

let count_id_conf = ref 0

let fresh_id_conf () =
  let x = !count_id_conf in
  count_id_conf := !count_id_conf + 1;x

type active_conf = 
  { id : id_conf;
    term : exprML;
    heap : Heap.heap;
    ienv : interactive_env;
    namectxP : name_ctx;
    namectxO : name_ctx }

type passive_conf = 
  { id : id_conf;
    heap : Heap.heap;
    ienv : interactive_env;
    namectxP : name_ctx;
    namectxO : name_ctx }

let negtype = function
  | Types.TArrow (ty1,ty2) -> (ty1,Types.TNeg ty2)
  | ty -> failwith ("Cannot negate the type " ^ (Types.string_of_typeML ty))

let p_trans aconf =
  let (nf,fenv,heap) = Eval_red.compute_nf (aconf.term,Pmap.empty,aconf.heap) in
  match Moves.decompose_nf (nf,fenv) with
  | IsCallExtern (fn,value,ectx) ->
    let id = fresh_id_conf () in
    let ty = Pmap.lookup_exn fn aconf.namectxP in
    let (ty1,ty2) = negtype ty in
    let (nup,ienv',lnamectx) = abstract_val value ty1 in
    let cn = fresh_cname () in
    let ienv = Pmap.concat ienv' aconf.ienv in
    let ienv = Pmap.add (cn,ectx) ienv in
    let lnamectx = Pmap.add (cn,ty2) lnamectx in
    (PQ (fn,nup,cn),
     {id = id; heap = heap; ienv = ienv; 
      namectxP = Pmap.concat lnamectx aconf.namectxP; namectxO = aconf.namectxO})
  | IsVal (cn,value) ->
    let id = fresh_id_conf () in
    let ty = Pmap.lookup_exn cn aconf.namectxP in
    let (nup,ienv,lnamectx) = abstract_val value ty in
    (PA (cn,nup),
     {id = id; heap = heap; ienv = Pmap.concat ienv aconf.ienv; 
      namectxP = Pmap.concat lnamectx aconf.namectxP; namectxO = aconf.namectxO})
  | IsRecCall _ -> failwith "OGS for recursive call is not yet implemented"

let o_trans pconf omove lnamectx =
  match omove with
  | OQ (fn,nup,cn) -> 
    begin match Pmap.lookup_pmap fn pconf.ienv with
    | Some value -> 
      let id = fresh_id_conf () in
      {id = id; term = Named (cn,App (value,nup)); heap = pconf.heap; ienv = pconf.ienv;
       namectxP = pconf.namectxP;
       namectxO = Pmap.concat lnamectx pconf.namectxP}
    | None -> failwith ""
    end
  | _ -> failwith ""

let string_of_active_conf aconf =
  "<" ^ Syntax.string_of_exprML aconf.term ^ " | " 
  ^ Heap.string_of_heap aconf.heap ^ " | "
  ^ string_of_interactive_env aconf.ienv
    ^ ">_" ^ string_of_int aconf.id

let string_of_passive_conf pconf =
  "<" ^ Heap.string_of_heap pconf.heap ^ " | "
  ^ string_of_interactive_env pconf.ienv ^ " | "
    ^ ">_" ^ string_of_int pconf.id