module type IBUILD = functor (M:Util.Monad.MONAD) (IntLTS : Strategy.LTS) -> sig

  type result = Success | Stopped 


  val interactive_build :
    show_move:(string -> unit) ->
    show_conf:(Yojson.Safe.t -> unit) ->
    show_moves_list:(Yojson.Safe.t list -> unit) ->
    (* the argument of get_move is the 
    number of moves *)
    get_move:(int -> int M.m) ->
    IntLTS.conf ->
    result M.m
end

module Make : IBUILD