module Make : functor (IntLTS : Bipartite.LTS) ->
  Graph.GRAPH with type conf = IntLTS.conf
