(*  This file contains signatures and implementation for various monads.*)

(** Standard signature for monads **)
module type MONAD = sig
  type 'a m

  val return : 'a -> 'a m
  val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m
end

module type RUNNABLE = sig
  include MONAD
  val run : 'a m -> 'a option
  val fail : unit -> 'a m
end

module Option = struct
  type 'a m = 'a option

  let return x = Some x
  let ( let* ) a f = match a with None -> None | Some x -> f x

  let run x = x

  let fail () = None
end

(** A simple branching monad **)

(*** Signature ***)
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

(*** List implementation of the Branching monad ***)
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


(*** An implementation of the Branching monad with user input to decide how to branch ***)
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

(** State monad **)

(*** Signature for the stored elements ***)
module type MEMSTATE = sig
  type t
end

(*** Signature for the State monad ***)
module type STATE = functor (State : MEMSTATE) -> sig
  type mem_state = State.t

  include MONAD with type 'a m = mem_state -> 'a * mem_state

  val get : unit -> mem_state m
  val set : mem_state -> unit m
  val runState : 'a m -> mem_state -> 'a * mem_state
end

(*** Standard implementation for the State monad ***)
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

(*** Signature for the combination of the Branching and State monad ***)
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

(** Write monad **)
(*** Used to produce trace of actions that can be printed ***)

(*** Signature for showable elements ***)
module type SHOWABLE = sig
  type t

  val show : t -> string
end

(*** Signature for the combination of Branching and Write monad ***)
module type BRANCH_WRITE = functor (MemState : SHOWABLE) -> sig
  type trace

  val string_of_trace : trace -> string

  include MONAD

  val emit : MemState.t -> unit m
  val fail : unit -> 'a m
  val para_list : 'a list -> 'a m
  val get_trace : 'a m -> trace list
end

(*** Implementation for the combination of Branching and Write monad using lists ***)
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

(*** Implementation for the combination of Branching and Write monad using user input ***)
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
