module type IBUILD = sig
  (* To be instanciated *)
  type conf

  (* Ajout du type result *)
  type result = Success | Stopped 

  (* *)

  val interactive_build :
    show_move:(string -> unit) ->
    show_conf:(Yojson.Safe.t -> unit) ->
    show_moves_list:(Yojson.Safe.t list -> unit) ->
    (* the argument of get_move is the 
    number of moves *)
    get_move:(int -> int Lwt.t) ->
    conf ->
    result Lwt.t (* Modification du type de retour ici (unit -> result) *)
end

module Make : functor (IntLTS : Strategy.LTS) ->
  IBUILD with type conf = IntLTS.conf