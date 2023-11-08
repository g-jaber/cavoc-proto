module type MONAD = sig
  type 'a m

  val return : 'a -> 'a m
  val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m
end

module type BRANCH = sig
  include MONAD

  val fail : unit -> 'a m
  val para_list : 'a list -> 'a m
  val run : 'a m -> 'a list
end

module ListB : BRANCH
module UserChoose : BRANCH

module type MEMSTATE = sig
  type t
end

module type STATE = functor (MemState : MEMSTATE) -> sig
  type mem_state = MemState.t

  include MONAD with type 'a m = mem_state -> 'a * mem_state

  val get : unit -> mem_state m
  val set : mem_state -> unit m
  val runState : 'a m -> mem_state -> 'a * mem_state
end

module State : STATE

module type BRANCH_STATE = functor (MemState : MEMSTATE) -> sig
  type mem_state = MemState.t

  include MONAD with type 'a m = mem_state -> 'a list * mem_state

  val get : unit -> mem_state m
  val set : mem_state -> unit m
  val fail : unit -> 'a m
  val para_list : 'a list -> 'a m
  val runState : 'a m -> mem_state -> 'a list * mem_state
end

module BranchState : BRANCH_STATE

module type SHOWABLE = sig
  type t

  val show : t -> string
end

module type BRANCH_WRITE = functor (MemState : SHOWABLE) -> sig
  type trace

  val string_of_trace : trace -> string

  include MONAD

  val emit : MemState.t -> unit m
  val fail : unit -> 'a m
  val para_list : 'a list -> 'a m
  val get_trace : 'a m -> trace list
end

module ListWrite : BRANCH_WRITE
module UserChooseWrite : BRANCH_WRITE
