module type IBUILD = functor (IntLTS : Strategy.LTS) -> sig

  type result = Success | Stopped 


  val interactive_build :
    show_move:(string -> unit) ->
    show_conf:(Yojson.Safe.t -> unit) ->
    show_moves_list:(Yojson.Safe.t list -> unit) ->
    (* the argument of get_move is the 
    number of moves *)
    get_move:(int -> int Lwt.t) ->
    IntLTS.conf ->
    result Lwt.t
end

module Make : IBUILD