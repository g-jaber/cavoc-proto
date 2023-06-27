open Logic

type arith_ctx = arith_pred list

let string_of_arith_ctx actx =
  let strlist = List.map Logic.string_of_arith_pred actx in
  let str = String.concat "/\\" strlist in
  "[" ^ str ^ "]"

type id_conf = int

(*
    let (aval,ienv) = abstract_val value in
    let cn = fresh_cname () in
    let ienv' = Pmap.add (cn,ectx) ienv in
    (fn,aval,ienv')
*)

let count_id_conf = ref 0

let fresh_id_conf () =
  let x = !count_id_conf in
  count_id_conf := !count_id_conf + 1;x

type active_conf = 
  { id : id_conf;
    term : Syntax.exprML;
    ground_var_ctx : Syntax.var_ctx;
    arith_ctx : arith_ctx }

type passive_conf = 
  { id : id_conf;
    ienv : Moves.interactive_env;
    ground_var_ctx : Syntax.var_ctx;
    arith_ctx : arith_ctx }

let string_of_active_conf aconf =
  "<" ^ Syntax.string_of_exprML aconf.term ^ " | " ^ 
    string_of_arith_ctx aconf.arith_ctx
    ^ ">_" ^ string_of_int aconf.id

let string_of_passive_conf pconf =
  "<" ^ Moves.string_of_interactive_env pconf.ienv ^ " | " ^ 
    string_of_arith_ctx pconf.arith_ctx
    ^ ">_" ^ string_of_int pconf.id

(*
let intern_trans aconf =
  let nf = Symb_red.compute_nf aconf.term in
  match nf with
*)