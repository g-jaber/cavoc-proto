module type GRAPH = sig
  (* To be instanciated *)
  type conf

  (* *)
  type graph

  val string_of_graph : graph -> string
  val compute_graph :
  show_move:(string -> unit) -> 
  show_conf:(string -> unit) -> 
  show_moves_list:(Yojson.Safe.t list -> unit) -> 
    (* the argument of get_move is the 
    number of moves *)
    get_move:(int -> int) 
  -> conf -> graph 
end

module Make : functor (IntLTS : Strategy.LTS) ->
  GRAPH with type conf = IntLTS.conf
