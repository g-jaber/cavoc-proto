module type IBUILD = sig
  (* To be instanciated *)
  type conf

  (* *)

  val interactive_build :
    show_move:(Yojson.Safe.t -> unit) ->
    show_conf:(Yojson.Safe.t -> unit) ->
    show_moves_list:(Yojson.Safe.t list -> unit) ->
    (* the argument of get_move is the 
    number of moves *)
    get_move:(int -> int Lwt.t) ->
    conf ->
    unit Lwt.t
end

module Make : functor (IntLTS : Strategy.LTS) ->
  IBUILD with type conf = IntLTS.conf
