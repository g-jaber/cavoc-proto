module Make :
  functor (IntLTS : Bipartite.INT_LTS) ->
    sig
      type event =
          Trans of IntLTS.Int.interactive_ctx * IntLTS.Actions.Moves.move
        | Leaf of IntLTS.Int.interactive_ctx
      val string_of_event : event -> string
      type trace
      type 'a m
      val return : 'a -> 'a m
      val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m
      val emit : event -> unit m
      val fail : unit -> 'a m
      val para_list : 'a list -> 'a m
      val get_trace : 'a m -> trace list
      val ask_print_conf : IntLTS.passive_conf -> unit
      val generate : IntLTS.conf -> unit m
      val string_of_trace : trace list -> string list
      val get_traces : IntLTS.conf -> string list
    end
