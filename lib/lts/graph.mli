module type GRAPH = sig
  (* To be instanciated *)
  module M : Util.Monad.MONAD

  type conf

  (* *)
  type graph

  val compute_graph :
    show_move:(string -> unit) ->
    show_conf:(Yojson.Safe.t -> unit) ->
    show_moves_list:(Yojson.Safe.t list -> unit) ->
    (* the argument of get_move is the 
    number of moves *)
    get_move:(int -> int M.m) ->
    conf ->
    graph M.m
end

module Make : functor (M : Util.Monad.MONAD) (IntLTS : Strategy.LTS) ->
  GRAPH with module M = M and type conf = IntLTS.conf
