(* The RefML part of definability: abstract values as patterns and values, the
   two matching forms, and printing synthesized programs in source syntax. *)

open Syntax

(* One reference allocation per location of the store. *)
let store_bindings store =
  List.map
    (fun (location, value) -> (location, Newref (Types.TUndef, value)))
    store

module Ops :
  Lang.Definability.OPLANG_OPS
    with type name = Names.name
     and type typ = Types.typ
     and type value = Syntax.value
     and type term = Syntax.term
     and type abstract_val = Nup.nup
     and type source_pattern = Syntax.pattern
     and type store = (Syntax.id * Syntax.term) list = struct
  type name = Names.name
  type typ = Types.typ
  type value = Syntax.value
  type term = Syntax.term
  type abstract_val = Nup.nup
  type source_pattern = Syntax.pattern

  (* Values by location; a location is a source identifier. *)
  type store = (Syntax.id * Syntax.term) list

  let rec value_of_abstract_val value_of_free_name value_of_bound_name =
    function
    | Nup.AUnit -> Unit
    | Nup.AInt n -> Int n
    | Nup.ABool b -> Bool b
    | Nup.APair (nup1, nup2) ->
        Pair
          ( value_of_abstract_val value_of_free_name value_of_bound_name nup1,
            value_of_abstract_val value_of_free_name value_of_bound_name nup2 )
    | Nup.ARecord fields ->
        Record
          (Util.Pmap.map_im
             (value_of_abstract_val value_of_free_name value_of_bound_name)
             fields)
    | Nup.AFree nn -> value_of_free_name nn
    | Nup.ABound nn -> value_of_bound_name nn
    | Nup.ACons _ ->
        failwith
          "Definability: exception values are outside the definable fragment."
    | Nup.ASymb _ ->
        failwith
          "Definability: symbolic values are outside the definable fragment."

  type branch = {
    pattern: abstract_val;
    guard: source_pattern option;
    identifier_of_bound_name: name -> string;
    body: (name -> value) -> term;
  }

  (* A nup as a source pattern, each bound name becoming the variable of its
     identifier. *)
  let rec pattern_of_abstract_val identifier_of_bound_name = function
    | Nup.AUnit -> PatUnit
    | Nup.AInt n -> PatInt n
    | Nup.ABool b -> PatBool b
    | Nup.APair (nup1, nup2) ->
        PatPair
          ( pattern_of_abstract_val identifier_of_bound_name nup1,
            pattern_of_abstract_val identifier_of_bound_name nup2 )
    | Nup.ABound nn -> PatVar (identifier_of_bound_name nn)
    | Nup.ARecord _ ->
        failwith "Definability: record patterns are not supported yet."
    | Nup.AFree _ ->
        failwith
          "Definability: a free name inside a pattern is a polymorphic name \
           given back, outside the definable fragment."
    | Nup.ACons _ ->
        failwith
          "Definability: exception patterns are outside the definable fragment."
    | Nup.ASymb _ ->
        failwith
          "Definability: symbolic patterns are outside the definable fragment."

  (*  A guarded match takes the pair of the matched term and the state reading
     term; unguarded branches pair with the wildcard. *)
  let match_on_patterns state_reading matched_term branches =
    let compile_branch pair_with_state_reading (branch : branch) =
      let pattern =
        pattern_of_abstract_val branch.identifier_of_bound_name branch.pattern
      in
      let pattern =
        if pair_with_state_reading then
          PatPair (pattern, Option.value ~default:PatWildcard branch.guard)
        else pattern in
      Handler
        ( pattern,
          branch.body (fun nn -> Var (branch.identifier_of_bound_name nn)) )
    in
    let guarded =
      List.exists (fun (branch : branch) -> Option.is_some branch.guard) in
    if guarded branches then begin
      match state_reading with
      | None ->
          failwith
            "Definability: a guarded branch without a state reading. Please \
             report."
      | Some guard_term ->
          Match
            ( Pair (matched_term, guard_term),
              List.map (compile_branch true) branches
              @ [ Handler (PatWildcard, Error) ] )
    end
    else
      Match
        ( matched_term,
          List.map (compile_branch false) branches
          @ [ Handler (PatWildcard, Error) ] )

  let pattern_matching_abstraction state_reading x input_typ branches =
    Fun ((x, input_typ), match_on_patterns state_reading (Var x) branches)

  let pattern_matching_call state_reading r value_of_free_name callee argument
      branches =
    Let
      ( r,
        App (value_of_free_name callee, argument),
        match_on_patterns state_reading (Var r) branches )

  let term_of_returned_value value = value

  let sequence prefix term =
    match prefix with Unit -> term | _ -> Seq (prefix, term)

  let allocate_store store term =
    List.fold_right
      (fun (location, allocation) body -> Let (location, allocation, body))
      (store_bindings store) term
end

(* The private declarations of a synthesized module, to lead its
   implementation. *)
let private_declarations_of_store = store_bindings

(* The reifiers of the ExtraMemory instances over this language; their
   machines live with the generic layer, and the assembly happens where
   both are visible. *)

module InnocentReification = struct
  let store : Ops.store = []
  let advance_term : Syntax.term = Syntax.Unit
  let guard_pattern : Syntax.pattern = Syntax.PatWildcard
  let state_reading_term : Syntax.term option = None
end

module ClockReification = struct
  let clock_location = "clockcell"
  let store clock : Ops.store = [ (clock_location, Syntax.Int clock) ]

  let advance_term : Syntax.term =
    Assign
      (Var clock_location, BinaryOp (Plus, Deref (Var clock_location), Int 1))

  let guard_pattern clock : Syntax.pattern = Syntax.PatInt clock

  let state_reading_term : Syntax.term option =
    Some (Deref (Var clock_location))
end

(* The higher-order-store reifiers: the clock cell plus one cell per
   provided name, indexed by providing order. *)
module HOSReification = struct
  let cell_location index = "providedcell" ^ string_of_int index

  (* The instance-owned default a cell is declared with, its name being
     provided only mid-play. *)
  let rec default_value : Types.typ -> Syntax.value = function
    | Types.TUnit -> Unit
    | Types.TInt -> Int 0
    | Types.TBool -> Bool false
    | Types.TProd (typ1, typ2) -> Pair (default_value typ1, default_value typ2)
    | Types.TArrow (typ1, typ2) -> Fun (("unused", typ1), default_value typ2)
    | typ ->
        failwith
          ("Definability: no default value at the type "
         ^ Types.string_of_typ typ ^ ".")

  let store provided_types clock : Ops.store =
    (ClockReification.clock_location, Syntax.Int clock)
    :: List.mapi
         (fun index typ -> (cell_location index, default_value typ))
         provided_types

  (* The advance of one occurrence: the clock increment, then each received value
     assigned to its cell. *)
  let advance_term assignments : Syntax.term =
    List.fold_left
      (fun code (cell_index, received_value) ->
        Seq (code, Assign (Var (cell_location cell_index), received_value)))
      ClockReification.advance_term assignments

  (* Let-bind the dereference of each read cell around the body, each read
     given with its cell and the identifier of its variable. *)
  let read_cells reads body : Syntax.term =
    let value_of_read read =
      match List.find_opt (fun (read', _, _) -> read' = read) reads with
      | Some (_, _, x) -> Syntax.Var x
      | None ->
          failwith "Definability: a read cell was not bound. Please report."
    in
    List.fold_right
      (fun (_, cell, x) code ->
        Syntax.Let (x, Deref (Var (cell_location cell)), code))
      reads (body value_of_read)

  let guard_pattern = ClockReification.guard_pattern
  let state_reading_term = ClockReification.state_reading_term
end

(* The extra memories over RefML in CPS: each machine with its reifiers. *)

module type CPS_MOVES =
  Lts.Moves.MOVES
    with type Renaming.Namectx.typ = (Types.negative_type, Types.typ) Either.t

module InnocentMemory (Moves : CPS_MOVES) = struct
  include Lts.Extra_memory.InnocentMachine (Moves)

  type oplang_store = Ops.store
  type oplang_term = Syntax.term
  type oplang_pattern = Syntax.pattern
  type oplang_value = Syntax.value

  let reify_store_declarations () = InnocentReification.store
  let reify_state () = InnocentReification.store

  let reify_advance _o_move InnocentGuard _ () =
    InnocentReification.advance_term

  let reify_pattern InnocentGuard = InnocentReification.guard_pattern
  let reify_state_reading () = InnocentReification.state_reading_term
  let reify_reads reads () body = Lts.Extra_memory.no_reads reads body
end

module ClockMemory (Moves : CPS_MOVES) = struct
  include Lts.Extra_memory.ClockMachine (Moves)

  type oplang_store = Ops.store
  type oplang_term = Syntax.term
  type oplang_pattern = Syntax.pattern
  type oplang_value = Syntax.value

  let reify_store_declarations _clock = ClockReification.store 0
  let reify_state clock = ClockReification.store clock
  let reify_advance _o_move (ClockAt _) _ _clock = ClockReification.advance_term
  let reify_pattern (ClockAt clock) = ClockReification.guard_pattern clock
  let reify_state_reading _clock = ClockReification.state_reading_term
  let reify_reads reads _clock body = Lts.Extra_memory.no_reads reads body
end

module HigherOrderStoreMemory (Moves : CPS_MOVES) = struct
  include Lts.Extra_memory.HOSMachine (Moves)
  module Namectx = Moves.Renaming.Namectx

  type oplang_store = Ops.store
  type oplang_term = Syntax.term
  type oplang_pattern = Syntax.pattern
  type oplang_value = Syntax.value

  (* The cell of a provided level is its position in the provided context,
     final-state knowledge. *)
  let cell_of_provided_level state read =
    let rec find index = function
      | [] -> failwith "Definability: not a provided level. Please report."
      | level :: levels ->
          if level = read then index else find (index + 1) levels in
    find 0 (Namectx.get_names (provided_context state))

  (* Provided names are functions, so their interactive type embeds an
     operational one. *)
  let provided_types state =
    List.map
      (fun (_name, typ) ->
        match typ with
        | Either.Left negative_type -> negative_type
        | Either.Right _ ->
            failwith "Definability: a continuation is never provided.")
      (List.concat state.provided_per_move)

  let reify_store_declarations state =
    HOSReification.store (provided_types state) 0

  (* The received names have no closed source form; the anchor keeps the
     clock, the definable part of the state. *)
  let reify_state state =
    HOSReification.store (provided_types state) state.clock

  (* The i-th name a move provides is received by its i-th non-continuation
     fresh name. *)
  let reify_advance o_move guard value_of_bound_name state =
    let receivers =
      List.filter
        (fun delta_name -> not (Namectx.Names.is_cname delta_name))
        (Moves.get_fresh_names o_move) in
    HOSReification.advance_term
      (List.map2
         (fun provided_level delta_name ->
           ( cell_of_provided_level state provided_level,
             value_of_bound_name delta_name ))
         (provided_levels_at guard state)
         receivers)

  let reify_pattern (ClockAt clock) = HOSReification.guard_pattern clock
  let reify_state_reading _state = HOSReification.state_reading_term

  let reify_reads reads state body =
    HOSReification.read_cells
      (List.map
         (fun read ->
           ( read,
             cell_of_provided_level state read,
             "provided_" ^ Namectx.Names.string_of_name read ))
         reads)
      body
end

(* Source printing: the emitted fragment in the grammar the parser accepts. *)

let rec pp_source_typ fmt = function
  | Types.TArrow (typ1, typ2) ->
      Format.fprintf fmt "%a -> %a" pp_source_prod_typ typ1 pp_source_typ typ2
  | typ -> pp_source_prod_typ fmt typ

and pp_source_prod_typ fmt = function
  | Types.TProd (typ1, typ2) ->
      Format.fprintf fmt "%a * %a" pp_source_prod_typ typ1 pp_source_atomic_typ
        typ2
  | typ -> pp_source_atomic_typ fmt typ

and pp_source_atomic_typ fmt = function
  | Types.TUnit -> Format.pp_print_string fmt "unit"
  | Types.TInt -> Format.pp_print_string fmt "int"
  | Types.TBool -> Format.pp_print_string fmt "bool"
  | Types.TExn -> Format.pp_print_string fmt "exn"
  | Types.TVar tvar -> Format.pp_print_string fmt tvar
  | Types.TId id -> Format.pp_print_string fmt id
  | Types.TRef typ -> Format.fprintf fmt "ref %a" pp_source_atomic_typ typ
  | Types.TRecord fields ->
      Format.pp_print_string fmt "{ ";
      Util.Pmap.iter
        (fun (id, typ) -> Format.fprintf fmt "%s : %a; " id pp_source_typ typ)
        fields;
      Format.pp_print_string fmt "}"
  | (Types.TArrow _ | Types.TProd _) as typ ->
      Format.fprintf fmt "(%a)" pp_source_typ typ
  | ( Types.TSum _ | Types.TAlgebraic _ | Types.TForall _ | Types.TName _
    | Types.TUndef ) as typ ->
      failwith
        ("Definability: the type " ^ Types.string_of_typ typ
       ^ " has no source form.")

let string_of_source_typ = Format.asprintf "%a" pp_source_typ

let rec pp_source_term fmt = function
  | Fun ((x, typ), body) ->
      Format.fprintf fmt "fun (%s : %a) -> %a" x pp_source_typ typ
        pp_source_term body
  | Let (x, bound, body) ->
      Format.fprintf fmt "let %s = %a in %a" x pp_source_term bound
        pp_source_term body
  | App (head, argument) ->
      Format.fprintf fmt "%a %a" pp_source_app_head head pp_source_atomic_term
        argument
  | term -> pp_source_atomic_term fmt term

and pp_source_app_head fmt = function
  | App (head, argument) ->
      Format.fprintf fmt "%a %a" pp_source_app_head head pp_source_atomic_term
        argument
  | term -> pp_source_atomic_term fmt term

and pp_source_atomic_term fmt = function
  | Var x -> Format.pp_print_string fmt x
  | Unit -> Format.pp_print_string fmt "()"
  | Int n when n >= 0 -> Format.pp_print_int fmt n
  | Int n -> Format.fprintf fmt "(0 - %d)" (-n)
  | Bool b -> Format.pp_print_bool fmt b
  | Error -> Format.pp_print_string fmt "(assert false)"
  | Pair (term1, term2) ->
      Format.fprintf fmt "(%a, %a)" pp_source_term term1 pp_source_term term2
  | Record fields ->
      Format.pp_print_string fmt "{ ";
      Util.Pmap.iter
        (fun (id, term) ->
          Format.fprintf fmt "%s = %a; " id pp_source_term term)
        fields;
      Format.pp_print_string fmt "}"
  | Match (matched_term, handler_l) ->
      let pp_sep fmt () = Format.pp_print_string fmt " " in
      let pp_handler fmt (Handler (pattern, body)) =
        Format.fprintf fmt "| %a -> %a" pp_pattern pattern pp_source_term body
      in
      Format.fprintf fmt "(match %a with %a)" pp_source_term matched_term
        (Format.pp_print_list ~pp_sep pp_handler)
        handler_l
  | Newref (_, term) -> Format.fprintf fmt "(ref %a)" pp_source_atomic_term term
  | Deref (Var x) -> Format.fprintf fmt "!%s" x
  | Assign (term1, term2) ->
      Format.fprintf fmt "(%a := %a)" pp_source_term term1 pp_source_term term2
  | Seq (term1, term2) ->
      Format.fprintf fmt "(%a; %a)" pp_source_term term1 pp_source_term term2
  | BinaryOp (Plus, term1, term2) ->
      Format.fprintf fmt "(%a + %a)" pp_source_term term1 pp_source_term term2
  | (Fun _ | Let _ | App _) as term ->
      Format.fprintf fmt "(%a)" pp_source_term term
  | term ->
      failwith
        ("Definability: the term " ^ string_of_term term
       ^ " is not part of the definable fragment.")

let string_of_source_term = Format.asprintf "%a" pp_source_term

(* In context order: initialization from source assigns levels in declaration order.
   The private declarations lead and stay out of any signature. *)
let source_of_definability_implementation ?(private_declarations = []) ~exports
    () =
  String.concat "\n"
    (List.map
       (fun (x, term) -> "let " ^ x ^ " = " ^ string_of_source_term term)
       (private_declarations @ exports))

let source_of_definability_module ?private_declarations ~exports ~imports () =
  let implementation =
    source_of_definability_implementation ?private_declarations
      ~exports:(List.map (fun (x, _typ, term) -> (x, term)) exports)
      () in
  let signature =
    String.concat "\n"
      (List.map
         (fun (x, typ, _term) -> "val " ^ x ^ " : " ^ string_of_source_typ typ)
         exports) in
  let import_signature =
    String.concat "\n"
      (List.map
         (fun (x, typ) -> "val " ^ x ^ " : " ^ string_of_source_typ typ)
         imports) in
  (implementation, signature, import_signature)

(* The same equations as {!RefML.WITHAVAL_INOUT_REFML}, for the implementation
   that also reifies moves into RefML source. *)
module type WITHAVAL_INOUT_DEFINABILITY_REFML = sig
  include RefML.WITHAVAL_INOUT_REFML module Definability : module type of Ops
end

module WithAValConcrete (BranchMonad : Util.Monad.BRANCH) :
  WITHAVAL_INOUT_DEFINABILITY_REFML
    with module EvalMonad = Util.Monad.SingleResult
     and module Store.BranchMonad = BranchMonad = struct
  include RefML.WithAValConcrete (BranchMonad) module Definability = Ops
end
