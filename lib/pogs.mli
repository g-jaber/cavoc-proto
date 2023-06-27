type arith_ctx = Logic.arith_pred list
type id_conf


val fresh_id_conf : unit -> id_conf


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

val string_of_active_conf : active_conf -> string
val string_of_passive_conf : passive_conf -> string