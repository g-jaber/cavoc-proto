(* The extra memory of a view-function strategy: the summary of the
   interaction beyond the P-view that Player moves may depend on, with its
   definability into the store of the programming language. *)
(* A strategy is innocent exactly when its extra memory is trivial. *)

module type EXTRA_MEMORY = sig
  type move
  type name
  type namectx
  type renaming

  (* The read guard discriminating the Player moves the P-view alone does not
     determine; compared with structural equality. *)
  type pattern
  type state

  val initial_state : state

  (* A P-view's guard is a function of the state alone: a program cannot
     discriminate on the identity of a name. *)
  val guard_of_state : state -> pattern

  (* Observe one Opponent move; what of it is remembered is the instance's
     own decision, read back only by its reifiers. *)
  val advance : move -> state -> state

  (* The provided context: the names the other player has provided that
     Player moves may use beyond their view, in providing order, with their
     types; it grows along the play and never reorders. *)
  val provided_context : state -> namectx

  (* The reading of the provided context in the ambient Opponent context. *)
  val provided_reading : state -> namectx -> renaming

  (* The provided levels contributed by the Opponent move answered at the
     occurrence this guard identifies: one block of the provided context. *)
  val provided_levels_at : pattern -> state -> name list
  val pp_pattern : Format.formatter -> pattern -> unit
  val pp_state : Format.formatter -> state -> unit
end

(* The reads of a memory that provides no names. *)
let no_reads reads body =
  match reads with
  | [] ->
      body (fun _name ->
          failwith "Extra_memory: no name was read. Please report.")
  | _ :: _ -> failwith "Extra_memory: this memory provides no names to read."

module InnocentMachine (Moves : Moves.MOVES) = struct
  type move = Moves.move
  type name = Moves.Renaming.Namectx.Names.name
  type namectx = Moves.Renaming.Namectx.t
  type renaming = Moves.Renaming.t
  type pattern = InnocentGuard
  type state = unit

  let initial_state = ()
  let guard_of_state () = InnocentGuard
  let advance _o_move () = ()
  let provided_context () = Moves.Renaming.Namectx.empty

  let provided_reading () namectxO =
    Moves.Renaming.tabulate Moves.Renaming.Namectx.empty namectxO Fun.id

  let provided_levels_at InnocentGuard () = []
  let pp_pattern fmt InnocentGuard = Format.pp_print_string fmt "⋄"
  let pp_state fmt () = Format.pp_print_string fmt "⋄"
end

module ClockMachine (Moves : Moves.MOVES) = struct
  type move = Moves.move
  type name = Moves.Renaming.Namectx.Names.name
  type namectx = Moves.Renaming.Namectx.t
  type renaming = Moves.Renaming.t
  type pattern = ClockAt of int
  type state = int

  let initial_state = 0
  let guard_of_state clock = ClockAt clock
  let advance _o_move clock = clock + 1
  let provided_context _clock = Moves.Renaming.Namectx.empty

  let provided_reading _clock namectxO =
    Moves.Renaming.tabulate Moves.Renaming.Namectx.empty namectxO Fun.id

  let provided_levels_at (ClockAt _) _clock = []
  let pp_pattern fmt (ClockAt clock) = Format.fprintf fmt "#%d" clock
  let pp_state fmt clock = Format.fprintf fmt "#%d" clock
end

(* The higher-order-store machine, remembering besides the clock the names
   each Opponent move provides, one block per move in providing order. *)
module HOSMachine (Moves : Moves.MOVES) = struct
  type move = Moves.move
  type name = Moves.Renaming.Namectx.Names.name
  type namectx = Moves.Renaming.Namectx.t
  type renaming = Moves.Renaming.t
  type pattern = ClockAt of int

  type state = {
    clock: int;
    provided_per_move: (name * Moves.Renaming.Namectx.typ) list list;
  }

  let initial_state = { clock= 0; provided_per_move= [] }
  let guard_of_state state = ClockAt state.clock

  (* Continuations are never provided: none can live in a store cell, and
     answering an out-of-view one breaks well-bracketing. *)
  let provided_of_move o_move =
    let local_namectx = Moves.get_namectx o_move in
    List.filter
      (fun (ambient_name, _typ) ->
        not (Moves.Renaming.Namectx.Names.is_cname ambient_name))
      (List.map2
         (fun ambient_name local_name ->
           ( ambient_name,
             Moves.Renaming.Namectx.lookup_exn local_namectx local_name ))
         (Moves.get_fresh_names o_move)
         (Moves.Renaming.Namectx.get_names local_namectx))

  let advance o_move state =
    {
      clock= state.clock + 1;
      provided_per_move= state.provided_per_move @ [ provided_of_move o_move ];
    }

  let provided_context state =
    List.fold_left
      (fun namectx (_ambient_name, typ) ->
        snd (Moves.Renaming.Namectx.add_fresh namectx "" typ))
      Moves.Renaming.Namectx.empty
      (List.concat state.provided_per_move)

  let provided_reading state namectxO =
    let provided_context = provided_context state in
    let ambient_names =
      List.combine
        (Moves.Renaming.Namectx.get_names provided_context)
        (List.map fst (List.concat state.provided_per_move)) in
    Moves.Renaming.tabulate provided_context namectxO (fun level ->
        List.assoc level ambient_names)

  (* The move answered at clock n contributed the n-th block. *)
  let provided_levels_at (ClockAt clock) state =
    let rec block_at clock levels = function
      | [] ->
          failwith
            "HOSMachine.provided_levels_at: the guard is beyond the recording. \
             Please report."
      | provided_by_move :: later_moves ->
          let size = List.length provided_by_move in
          if clock = 0 then List.filteri (fun index _ -> index < size) levels
          else
            block_at (clock - 1)
              (List.filteri (fun index _ -> index >= size) levels)
              later_moves in
    block_at clock
      (Moves.Renaming.Namectx.get_names (provided_context state))
      state.provided_per_move

  let pp_pattern fmt (ClockAt clock) = Format.fprintf fmt "#%d" clock

  let pp_state fmt state =
    let pp_provided fmt (name, _typ) =
      Moves.Renaming.Namectx.Names.pp_name fmt name in
    Format.fprintf fmt "@[#%d providing %a@]" state.clock
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
         pp_provided)
      (List.concat state.provided_per_move)
end
