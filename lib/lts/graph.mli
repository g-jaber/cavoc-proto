module type GRAPH = sig
  (* To be instanciated *)
  module UserMonad : Util.Monad.MONAD

  type conf

  (* *)
  type graph

  val compute_graph :
    show_move:(string -> unit) ->
    show_conf:(Yojson.Safe.t -> unit) ->
    show_moves_list:(Yojson.Safe.t list -> unit) ->
    (* the argument of get_move is the 
    number of moves *)
    get_move:(int -> int UserMonad.m) ->
    conf ->
    graph UserMonad.m
end

module Make : functor (UserMonad : Util.Monad.MONAD) (IntLTS : Strategy.LTS) ->
  GRAPH with module UserMonad = UserMonad and type conf = IntLTS.conf
