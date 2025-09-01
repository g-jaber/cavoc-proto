module Make : functor (IntLTS : Strategy.LTS) ->
  Graph.GRAPH with type conf = IntLTS.conf
