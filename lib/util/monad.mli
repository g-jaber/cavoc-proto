module type MONAD = sig
  type 'a m
  val return : 'a -> 'a m
  val ( let* ) :
    'a m -> ('a -> 'b m) -> 'b m
end

module type LISTMONAD = sig
  include MONAD
  val fail : unit -> 'a m
  val para_list : 'a list -> 'a m
  val run : 'a m -> 'a list
end
  

module ListMonad : LISTMONAD


module type MEMSTATE = sig type t end

module type STMONAD = functor (MemState : MEMSTATE) ->
  sig
    type mem_state = MemState.t
    include MONAD with type 'a m = mem_state -> ('a * mem_state)
    val get : unit -> mem_state m
    val set : mem_state -> unit m
    val runState : 'a m -> mem_state -> ('a* mem_state)
  end

module StMonad : STMONAD

module type LSTMONAD = functor (MemState : MEMSTATE) ->
  sig
    type mem_state = MemState.t
    include MONAD with type 'a m = mem_state -> ('a list * mem_state)
    val get : unit -> mem_state m
    val set : mem_state -> unit m
    val fail : unit -> 'a m
    val para_list : 'a list -> 'a m
    val runState : 'a m -> mem_state -> ('a list * mem_state)
  end

module LStMonad : LSTMONAD

module type LWMONAD = functor (MemState : MEMSTATE) ->
  sig
    type output = MemState.t
    include MONAD with type 'a m = ('a * output list) list
    val print : output -> unit m
    val fail : unit -> 'a m
    val para_list : 'a list -> 'a m
    val get_output : 'a m -> output list list
  end

module LWMonad : LWMONAD
