(**
  This module contains the signatures and implementations of monads used throughout
  {e CAVOC}.
 *)

(** Standard signature for monads. *)
module type MONAD = sig
  type 'a m

  val return : 'a -> 'a m
  val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m
end

(** {2 Runnable monad} *)

(**
  Standard signature for evaluation monads.
  
  Monads implementing [RUNNABLE] are used by {!page-index.machinelanguages}
  to encapsulate the return type of the interpreter.
  *)
module type RUNNABLE = sig
  include MONAD

  (**
     Result of evaluation of an active configuration.
   *)
  type 'a result = 
    | PropStop (** This means that the proponent (i.e. the module) has stopped playing. *)
    | PropDiverges
    | Continue of 'a (** The interpreter successfuly returned some configuration(s). *)

  (**
    This type encapsulates one or more {!result}.

    {!page-index.machinelanguages} are responsible for choosing an appropriate
    implementation of [RUNNABLE] and exposing its type [r] via module
    constraints. Type [r] is then automatically propagated upwards by functors
    tranforming machine languages.
   *)
  type 'a r

  val run : 'a m -> 'a result r
  val stop : unit -> 'a m
end

(**
  Multi-result implementation of {!module-type: RUNNABLE}.

  See {!type: RUNNABLE.r} for details about type {!type: Result.r}
  *)
module Result = struct
  type 'a result = 
    | PropStop
    | PropDiverges
    | Continue of 'a
  type 'a m = 'a result list

  type 'a r = 'a list

  let return x : 'a m =
    [ Continue x ]

  let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m  =
    let bs = List.map
      (function
        | Continue x -> f x
        | PropStop -> [ PropStop ]
        | PropDiverges -> [ PropDiverges ]) a
    in
    List.concat bs

  let run x = x

  let stop () =
    [ PropStop ]
end


(**
  Single-result implementation of {!module-type: RUNNABLE}.

  See {!type: RUNNABLE.r} for details about type {!type: SingleResult.r}
  *)
module SingleResult = struct
  type 'a result =
    | PropStop
    | PropDiverges
    | Continue of 'a
  type 'a m = 'a result
  type 'a r = 'a

  let return x : 'a m =
    Continue x

  let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m =
    (function
      | Continue x -> f x
      | PropStop -> PropStop
      | PropDiverges -> PropDiverges) a

  let run a : 'a r =
    a

  let stop () =
    PropStop
end

module Option = struct
  type 'a m = 'a option

  let return x = Some x
  let ( let* ) a f = match a with None -> None | Some x -> f x

  let run x = x

  let fail () = None
end

(** {2 Branching monad} *)


(** Standard branching monad signature. *)
module type BRANCH = sig
  include MONAD

  val fail : unit -> 'a m

  val para_pair : 'a m -> 'a m -> 'a m
  val para_list : 'a list -> 'a m
  val pick_int : unit -> int m
  (* The function pick_elem provide a way to pick an element in a list. Its first argument is a function that is used to print the various elements of the list. 
  val pick_elem : ('a -> string) -> 'a list -> 'a*)
  val run : 'a m -> 'a list
end

(** List implementation of the Branching monad *)
module ListB : BRANCH = struct
  type 'a m = 'a list

  let return x = [ x ]
  let ( let* ) a f = List.flatten (List.map f a)
  let fail () = []

  let para_pair a b = a@b
  let para_list l = l
  let max_int = 3
  let pick_int () = List.init max_int (fun i -> i)
  let run a = a
end


(** An implementation of the Branching monad with user input to decide how to branch *)
module UserChoose : BRANCH = struct
  type 'a m = 'a option

  let return x = Some x
  let ( let* ) a f = match a with None -> None | Some x -> f x
  let fail () = None

  let para_pair a b =
      print_endline
        ("Choose an integer between 1 and 2, or choose any other integer to stop.");
      match read_int () with
        | 1 -> a
        | 2 -> b
        | _ -> None

  let para_list = function
    | [] -> None
    | l ->
        let n = List.length l in
        print_endline
          ("Choose an integer between 1 and " ^ string_of_int n
         ^ " to decide what to do, or choose 0 to stop.");
        let i = read_int () in
        if i > 0 && i <= n then Some (List.nth l (i - 1)) else None

  let pick_int () = 
    print_endline
    ("Choose an integer");
    let i = read_int () in
    Some i

  let run = function None -> [] | Some x -> [ x ]
end

(** {2 State monad} *)

(** Signature for the stored elements *)
module type MEMSTATE = sig
  type t
end

(** Standard state monad signature. *)
module type STATE = functor (State : MEMSTATE) -> sig
  type mem_state = State.t

  include MONAD with type 'a m = mem_state -> 'a * mem_state

  val get : unit -> mem_state m
  val set : mem_state -> unit m
  val runState : 'a m -> mem_state -> 'a * mem_state
end

(** Standard implementation for the State monad *)
module State : STATE =
functor
  (MemState : MEMSTATE)
  ->
  struct
    type mem_state = MemState.t
    type 'a m = mem_state -> 'a * mem_state

    let get () : mem_state m = fun st -> (st, st)
    let set st : unit m = fun _ -> ((), st)
    let runState (expr : 'a m) (st : mem_state) : 'a * mem_state = expr st
    let return (value : 'a) : 'a m = fun st -> (value, st)

    let ( let* ) (expr : 'a m) (f : 'a -> 'b m) : 'b m =
     fun st ->
      let (avalues, st') = runState expr st in
      runState (f avalues) st'
  end

(** Signature for the combination of the Branching and State monad *)
module type BRANCH_STATE = functor (State : MEMSTATE) -> sig
  type mem_state = State.t

  include MONAD with type 'a m = mem_state -> 'a list * mem_state

  val get : unit -> mem_state m
  val set : mem_state -> unit m
  val fail : unit -> 'a m
  val para_list : 'a list -> 'a m
  val runState : 'a m -> mem_state -> 'a list * mem_state
end

module BranchState : BRANCH_STATE =
functor
  (MemState : MEMSTATE)
  ->
  struct
    type mem_state = MemState.t
    type 'a m = mem_state -> 'a list * mem_state

    let get () : mem_state m = fun st -> ([ st ], st)
    let set st : unit m = fun _ -> ([ () ], st)
    let fail () : 'a m = fun st -> ([], st)
    let para_list l st = (l, st)
    let runState (expr : 'a m) (st : mem_state) : 'a list * mem_state = expr st
    let return (value : 'a) : 'a m = fun st -> ([ value ], st)

    let ( let* ) (expr : 'a m) (f : 'a -> 'b m) : 'b m =
     fun st ->
      let (avalues, st') = runState expr st in
      let rec aux avalues st =
        match avalues with
        | [] -> ([], st)
        | aval :: avalues' ->
            let (bvalues, st') = runState (f aval) st in
            let (bvalues', st'') = aux avalues' st' in
            (bvalues @ bvalues', st'') in
      aux avalues st'
  end

(** {2 Output monad} *)

(** Signature for showable elements *)
module type SHOWABLE = sig
  type t

  val show : t -> string
end
module type OUTPUT = functor (MemState : SHOWABLE) -> sig
  type event

  val string_of_event : event -> string

  include MONAD

  val emit : MemState.t -> unit m
  val get_trace : 'a m -> event list
end

(**
  Write monad.

  Used to produce a trace of actions that can be printed.
 *)
module Output : OUTPUT =
functor
  (MemState : SHOWABLE)
  ->
  struct
    type event = MemState.t list

    let string_of_event event =
      String.concat "·" @@ List.map MemState.show event

    type 'a m = 'a * event

    let emit out : unit m = ((), [ out ])

    let return (value : 'a) : 'a m = (value, [])
    let get_trace (_, tr) = [ tr ]

    let ( let* ) (x,tr) f =
          let (y,tr')  = f x in (y, tr @ tr')
  end

(** Signature for the combination of Branching and Write monad *)
module type BRANCH_WRITE = functor (MemState : SHOWABLE) -> sig
  type trace

  val string_of_trace : trace -> string

  include MONAD

  val emit : MemState.t -> unit m
  val fail : unit -> 'a m
  val para_list : 'a list -> 'a m
  val get_trace : 'a m -> trace list
end

(** Implementation for the combination of Branching and Write monad using lists *)
module ListWrite : BRANCH_WRITE =
functor
  (MemState : SHOWABLE)
  ->
  struct
    type trace = MemState.t list

    let string_of_trace trace =
      String.concat "·" @@ List.map MemState.show trace

    type 'a m = ('a * trace) list

    let emit out : unit m = [ ((), [ out ]) ]
    let fail () : 'a m = []
    let para_list l = List.map (fun a -> (a, [])) l
    let return (value : 'a) : 'a m = [ (value, []) ]
    let get_trace (expr : 'a m) : trace list = List.map snd expr

    let ( let* ) (expr : 'a m) (f : 'a -> 'b m) : 'b m =
      let add_out out (a, out') = (a, out' @ out) in
      let flift (a, out) = List.map (add_out out) (f a) in
      List.flatten (List.map flift expr)
  end

(** Implementation for the combination of Branching and Write monad using user input *)
module UserChooseWrite : BRANCH_WRITE =
functor
  (MemState : SHOWABLE)
  ->
  struct
    type trace = MemState.t list

    let string_of_trace trace =
      String.concat "·" @@ List.map MemState.show trace

    type 'a m = 'a option * trace

    let emit out : unit m = (Some (), [ out ])
    let fail () : 'a m = (None, [])

    let para_list = function
      | [] -> (None, [])
      | l ->
          let n = List.length l in
          print_endline
            ("Choose an integer between 1 and " ^ string_of_int n
           ^ " or choose 0 to stop.");
          let i = read_int () in
          if i > 0 && i <= n then (Some (List.nth l (i - 1)), []) else (None, [])

    let return (value : 'a) : 'a m = (Some value, [])
    let get_trace (_, tr) = [ tr ]

    let ( let* ) a f =
      match a with
      | (None, tr) -> (None, tr)
      | (Some x, tr) -> begin
          match f x with
          | (None, tr') -> (None, tr @ tr')
          | (Some y, tr') -> (Some y, tr @ tr')
        end
  end
