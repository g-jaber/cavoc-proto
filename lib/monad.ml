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
  

module ListMonad : LISTMONAD = struct
  type 'a m = 'a list
  let return x = [x]
  let ( let* ) a f = List.flatten (List.map f a)
  let fail () = []
  let para_list l = l
  let run a = a
end

module type MEMSTATE = sig type t end

module type STMONAD = functor (State : MEMSTATE) ->
  sig
    type mem_state = State.t
    include MONAD with type 'a m = mem_state -> ('a * mem_state)
    val get : unit -> mem_state m
    val set : mem_state -> unit m
    val runState : 'a m -> mem_state -> ('a * mem_state)
  end

module type LSTMONAD = functor (State : MEMSTATE) ->
  sig
    type mem_state = State.t
    include MONAD with type 'a m = mem_state -> ('a list * mem_state)
    val get : unit -> mem_state m
    val set : mem_state -> unit m
    val fail : unit -> 'a m
    val para_list : 'a list -> 'a m
    val runState : 'a m -> mem_state -> ('a list * mem_state)
  end

module StMonad : STMONAD = functor (MemState : MEMSTATE) -> struct
  type mem_state = MemState.t
  type 'a m  = mem_state -> ('a * mem_state)

  let get () : mem_state m = fun st -> (st,st)

  let set st : unit m = fun _ -> ((),st)


  let runState (expr:'a m) (st:mem_state) : ('a * mem_state) =
    expr st
  let return (value:'a) : 'a m = fun st -> (value,st)

  let (let*) (expr:'a m) (f:'a -> 'b m) : 'b m = fun st ->
    let (avalues,st') = runState expr st in
    runState (f avalues) st'
end

module LStMonad : LSTMONAD = functor (MemState : MEMSTATE) -> struct
  type mem_state = MemState.t
  type 'a m  = mem_state -> ('a list * mem_state)

  let get () : mem_state m = fun st -> ([st],st)

  let set st : unit m = fun _ -> ([()],st)

  let fail () : 'a m = fun st -> ([],st)

  let para_list l = fun st -> (l,st)

  let runState (expr:'a m) (st:mem_state) : ('a list * mem_state) =
    expr st

  let return (value:'a) : 'a m = fun st -> ([value],st)

  let (let*) (expr:'a m) (f:'a -> 'b m) : 'b m = fun st ->
    let (avalues,st') = runState expr st in
    let rec aux avalues st = 
      match avalues with
      | [] -> ([],st)
      | aval::avalues' -> 
        let (bvalues, st') = runState (f aval) st in
        let (bvalues', st'') = aux avalues' st' in
        (bvalues@bvalues',st'')
    in aux avalues st'
end

module type LWMONAD = functor (MemState : MEMSTATE) ->
  sig
    type output = MemState.t
    include MONAD with type 'a m = ('a * output list) list
    val print : output -> unit m
    val fail : unit -> 'a m
    val para_list : 'a list -> 'a m
    val get_output : 'a m -> output list list
  end

module LWMonad : LWMONAD = functor (MemState : MEMSTATE) -> struct
  type output = MemState.t
  type 'a m  = ('a * output list) list
  let print out : unit m = ([(),[out]])

  let fail () : 'a m = []

  let para_list l = List.map (fun a -> (a,[])) l

  let return (value:'a) : 'a m = ([(value,[])])

  let get_output (expr: 'a m) : (output list) list =
    List.map snd expr

  let (let*) (expr:'a m) (f:'a -> 'b m) : 'b m =
    let add_out out (a,out') = (a,out'@out) in
    let flift (a,out) = List.map (add_out out) (f a)
    in List.flatten (List.map flift expr)
end