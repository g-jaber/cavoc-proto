module type IBUILD = sig
  (* To be instanciated *)
  type conf

  (* *)

  val interactive_build :
    show_move:(string -> unit) ->
    show_conf:(Yojson.Safe.t -> unit) ->
    show_moves_list:(string list -> unit) ->
    (* the argument of get_move is the 
    number of moves *)
    get_move:(int -> int Lwt.t) ->
    conf ->
    unit Lwt.t
end

module Make : functor (IntLTS : Bipartite.LTS) ->
  IBUILD with type conf = IntLTS.conf
