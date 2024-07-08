module type GRAPH = sig
  (* To be instanciated *)
  type conf

  (* *)
  type graph

  val string_of_graph : graph -> string
  val compute_graph : conf -> graph
end

module Make : functor (IntLTS : Bipartite.LTS) ->
  GRAPH with type conf = IntLTS.conf
