type id_conf = int
type nup = Syntax.exprML
val count_fname : id_conf ref
val fresh_fname : unit -> string
val count_cname : id_conf ref
val fresh_cname : unit -> string
val count_id_conf : id_conf ref
val fresh_id_conf : unit -> id_conf

type active_conf = 
  { id : id_conf;
    term : Syntax.exprML;
    heap : Heap.heap;
    ienv : Moves.interactive_env;
    namectxP : Moves.name_ctx;
    namectxO : Moves.name_ctx }

type passive_conf = 
  { id : id_conf;
    heap : Heap.heap;
    ienv : Moves.interactive_env;
    namectxP : Moves.name_ctx;
    namectxO : Moves.name_ctx }

val p_trans : active_conf -> Moves.action * passive_conf
val o_trans : passive_conf -> Moves.action -> Moves.name_ctx -> active_conf
val string_of_active_conf : active_conf -> string
val string_of_passive_conf : passive_conf -> string
