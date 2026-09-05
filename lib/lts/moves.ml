module type MOVES = sig
  (* to be instantiated *)
  module Renaming : Lang.Renaming.INJECTIVE_RENAMING

  type copattern

  (* A move is exactly its copattern; the subject is read from the abstract
     normal form inside it (get_subject_name), not stored alongside. *)
  type move = copattern [@@deriving to_yojson]

  val pp_move : Format.formatter -> move -> unit
  val string_of_move : move -> string

  (* The _in variants display the free names with show_name, typically
     Namectx.show_name_in of the ambient context.
    The bound names are shown at their
     level using the given weakening of the local context. *)
  val pp_move_in :
    show_name:(Renaming.Namectx.Names.name -> string) ->
    Renaming.t ->
    Format.formatter ->
    move ->
    unit

  val string_of_move_in :
    show_name:(Renaming.Namectx.Names.name -> string) ->
    Renaming.t ->
    move ->
    string

  val move_to_yojson_in :
    show_name:(Renaming.Namectx.Names.name -> string) ->
    Renaming.t ->
    move ->
    Yojson.Safe.t

  val get_subject_name : move -> Renaming.Namectx.Names.name

  (* The local context typing the names introduced by the move;
     its names are move-local levels. *)
  val get_namectx : move -> Renaming.Namectx.t

  (* The names introduced by the move, computed via the weakening of its local
     context into the ambient one. *)
  val fresh_names : Renaming.t -> move -> Renaming.Namectx.Names.name list

  (* Rename the free names of the move, including the subject. *)
  val map_free_names :
    (Renaming.Namectx.Names.name -> Renaming.Namectx.Names.name) -> move -> move

  (* The move with the display hints of its local context erased, so that
     stored moves compare structurally. *)
  val erase_display_hints : move -> move

  (* Names being de Bruijn levels, moves at the same position compare structurally. *)
  val is_equiv_move : move -> move -> bool
end

module type POLMOVES = sig
  include MOVES

  type direction = Input | Output
  type pol_move = direction * move [@@deriving to_yojson]

  val yojson_of_move : move -> Yojson.Safe.t
  val pp_pol_move : Format.formatter -> pol_move -> unit
  val string_of_pol_move : pol_move -> string

  val pp_pol_move_in :
    show_name:(Renaming.Namectx.Names.name -> string) ->
    Renaming.t ->
    Format.formatter ->
    pol_move ->
    unit

  val string_of_pol_move_in :
    show_name:(Renaming.Namectx.Names.name -> string) ->
    Renaming.t ->
    pol_move ->
    string

  val pol_move_to_yojson_in :
    show_name:(Renaming.Namectx.Names.name -> string) ->
    Renaming.t ->
    pol_move ->
    Yojson.Safe.t

  val switch_direction : pol_move -> pol_move

  val is_equiv_pol_move : pol_move -> pol_move -> bool
end

module type GEN_POLMOVES = sig
  include POLMOVES
  module BranchMonad : Util.Monad.BRANCH

  val generate_moves :
    Renaming.Namectx.t -> (move * Renaming.Namectx.t) BranchMonad.m

  val infer_type_move : Renaming.Namectx.t -> move -> Renaming.Namectx.t option
  val check_type_move : Renaming.Namectx.t -> move * Renaming.Namectx.t -> bool
end

module type GEN_MOVES = sig
  include MOVES
  module BranchMonad : Util.Monad.BRANCH

  val generate_moves :
    Renaming.Namectx.t -> (move * Renaming.Namectx.t) BranchMonad.m

  val infer_type_move : Renaming.Namectx.t -> move -> Renaming.Namectx.t option
  val check_type_move : Renaming.Namectx.t -> move * Renaming.Namectx.t -> bool
end

(* module POLARIZE (Moves : MOVES) : POLMOVES = struct
include Moves

  type direction = Input | Output
  type pol_move = direction * move

  let fmt pp_pol_move : Format.formatter -> pol_move -> unit
  val string_of_pol_move : pol_move -> string
  val switch_direction : pol_move -> pol_move

  val is_equiv_pol_move :
end *)

(* The typing LTS fixes whether the heap part of moves is compared. *)
module Make
    (A_nf : Lang.Interactive.A_NF)
    (Equivalence : sig
      val compare_heaps : bool
    end) :
  POLMOVES
    with module Renaming = A_nf.IEnv.Renaming
     and type copattern =
      A_nf.abstract_normal_form * A_nf.IEnv.Renaming.Namectx.t = struct
  module Renaming = A_nf.IEnv.Renaming
  module Namectx = Renaming.Namectx

  type copattern = A_nf.abstract_normal_form * Namectx.t
  type move = copattern
  type direction = Input | Output [@@deriving to_yojson]

  let string_of_direction = function Input -> "?" | Output -> "!"
  let switch = function Input -> Output | Output -> Input

  type pol_move = direction * move

  let pp_move_gen ~show_name ~show_bound_name ~pp_dir fmt (a_nf, _) =
    let pp_free_name fmt nn = Format.pp_print_string fmt (show_name nn) in
    let pp_bound_name fmt nn = Format.pp_print_string fmt (show_bound_name nn) in
    A_nf.pp_a_nf_in ~pp_dir ~pp_free_name ~pp_bound_name fmt a_nf

  let default_show = Namectx.Names.string_of_name
  let show_through weakening nn = default_show (Renaming.lookup weakening nn)
  let no_dir fmt = Format.pp_print_string fmt ""

  let pp_move_in ~show_name weakening fmt move =
    pp_move_gen ~show_name ~show_bound_name:(show_through weakening)
      ~pp_dir:no_dir fmt move

  (* Bound names are displayed at their local levels. *)
  let pp_move fmt move =
    pp_move_gen ~show_name:default_show ~show_bound_name:default_show
      ~pp_dir:no_dir fmt move

  let string_of_move_in ~show_name weakening =
    Format.asprintf "%a" (pp_move_in ~show_name weakening)

  let string_of_move = Format.asprintf "%a" pp_move

  let pp_pol_move_in ~show_name weakening fmt (dir, move) =
    let pp_dir fmt = Format.pp_print_string fmt (string_of_direction dir) in
    pp_move_gen ~show_name ~show_bound_name:(show_through weakening) ~pp_dir fmt
      move

  let pp_pol_move fmt (dir, move) =
    let pp_dir fmt = Format.pp_print_string fmt (string_of_direction dir) in
    pp_move_gen ~show_name:default_show ~show_bound_name:default_show ~pp_dir
      fmt move

  let string_of_pol_move_in ~show_name weakening =
    Format.asprintf "%a" (pp_pol_move_in ~show_name weakening)

  let string_of_pol_move = Format.asprintf "%a" pp_pol_move

  let move_to_yojson_in ~show_name weakening ((a_nf, _) as move : move) =
    `Assoc
      [
        ("subjectName", `String (show_name (A_nf.get_subject_name a_nf)));
        ("string", `String (string_of_move_in ~show_name weakening move));
      ]

  let move_to_yojson ((a_nf, _) as move : move) =
    `Assoc
      [
        ("subjectName", `String (default_show (A_nf.get_subject_name a_nf)));
        ("string", `String (string_of_move move));
      ]

  let pol_move_to_yojson_in ~show_name weakening (_, move) =
    move_to_yojson_in ~show_name weakening move

  let pol_move_to_yojson (_, move) = move_to_yojson move

  let yojson_of_move (m : move) : Yojson.Safe.t =
    `Assoc [ ("label", `String (string_of_move m)) ]

  let switch_direction (p, d) = (switch p, d)
  let get_subject_name (a_nf, _) = A_nf.get_subject_name a_nf
  let get_namectx (_, local_namectx) = local_namectx

  let fresh_names weakening (_, local_namectx) =
    List.map (Renaming.lookup weakening) (Namectx.get_names local_namectx)

  let map_free_names f (a_nf, local_namectx) =
    (A_nf.map_free_names_of_a_nf f a_nf, local_namectx)

  let erase_display_hints (a_nf, local_namectx) =
    (a_nf, Namectx.erase_display_hints local_namectx)

  let is_equiv_move (a_nf1, _) (a_nf2, _) =
    A_nf.is_equiv_a_nf ~compare_heaps:Equivalence.compare_heaps a_nf1 a_nf2

  let is_equiv_pol_move (dir1, move1) (dir2, move2) =
    dir1 = dir2 && is_equiv_move move1 move2
end

module Tensor (Moves1 : POLMOVES) (Moves2 : POLMOVES) :
  POLMOVES
    with type copattern = (Moves1.copattern, Moves2.copattern) Either.t
     and type direction = Moves1.direction
     and type Renaming.t = Moves1.Renaming.t * Moves2.Renaming.t
     and type Renaming.Namectx.t =
      Moves1.Renaming.Namectx.t * Moves2.Renaming.Namectx.t
     and type Renaming.Namectx.typ =
      (Moves1.Renaming.Namectx.typ, Moves2.Renaming.Namectx.typ) Either.t
     and type Renaming.Namectx.Names.name =
      ( Moves1.Renaming.Namectx.Names.name,
        Moves2.Renaming.Namectx.Names.name )
      Either.t = struct
  module Names =
    Lang.Names.MakeAggregate
      (Moves1.Renaming.Namectx.Names)
      (Moves2.Renaming.Namectx.Names)

  module Namectx =
    Lang.Typectx.Aggregate (Moves1.Renaming.Namectx) (Moves2.Renaming.Namectx)
      (Names)

  module Renaming =
    Lang.Renaming.AggregateInjectiveRenaming (Moves1.Renaming) (Moves2.Renaming)
      (Namectx)

  type copattern = (Moves1.copattern, Moves2.copattern) Either.t
  type move = copattern
  type direction = Moves1.direction = Input | Output
  type pol_move = direction * move

  let switch = function Input -> Output | Output -> Input
  let switch_direction (dir, move) = (switch dir, move)

  (* The direction type of the second typing LTS is its own. *)
  let direction2 = function Input -> Moves2.Input | Output -> Moves2.Output
  let show_name1 show_name nn = show_name (Either.Left nn)
  let show_name2 show_name nn = show_name (Either.Right nn)

  let pp_move_in ~show_name (weakening1, weakening2) fmt = function
    | Either.Left move ->
        Moves1.pp_move_in ~show_name:(show_name1 show_name) weakening1 fmt move
    | Either.Right move ->
        Moves2.pp_move_in ~show_name:(show_name2 show_name) weakening2 fmt move

  let pp_move fmt = function
    | Either.Left move -> Moves1.pp_move fmt move
    | Either.Right move -> Moves2.pp_move fmt move

  let string_of_move_in ~show_name weakening =
    Format.asprintf "%a" (pp_move_in ~show_name weakening)

  let string_of_move = Format.asprintf "%a" pp_move

  let pp_pol_move_in ~show_name (weakening1, weakening2) fmt (dir, move) =
    match move with
    | Either.Left move ->
        Moves1.pp_pol_move_in ~show_name:(show_name1 show_name) weakening1 fmt
          (dir, move)
    | Either.Right move ->
        Moves2.pp_pol_move_in ~show_name:(show_name2 show_name) weakening2 fmt
          (direction2 dir, move)

  let pp_pol_move fmt (dir, move) =
    match move with
    | Either.Left move -> Moves1.pp_pol_move fmt (dir, move)
    | Either.Right move -> Moves2.pp_pol_move fmt (direction2 dir, move)

  let string_of_pol_move_in ~show_name weakening =
    Format.asprintf "%a" (pp_pol_move_in ~show_name weakening)

  let string_of_pol_move = Format.asprintf "%a" pp_pol_move

  let move_to_yojson_in ~show_name (weakening1, weakening2) = function
    | Either.Left move ->
        `Assoc
          [
            ( "left",
              Moves1.move_to_yojson_in ~show_name:(show_name1 show_name)
                weakening1 move );
          ]
    | Either.Right move ->
        `Assoc
          [
            ( "right",
              Moves2.move_to_yojson_in ~show_name:(show_name2 show_name)
                weakening2 move );
          ]

  let move_to_yojson = function
    | Either.Left move -> `Assoc [ ("left", Moves1.move_to_yojson move) ]
    | Either.Right move -> `Assoc [ ("right", Moves2.move_to_yojson move) ]

  let pol_move_to_yojson_in ~show_name weakening (_, move) =
    move_to_yojson_in ~show_name weakening move

  let pol_move_to_yojson (_, move) = move_to_yojson move

  let yojson_of_move (move : move) : Yojson.Safe.t =
    `Assoc [ ("label", `String (string_of_move move)) ]

  let get_subject_name = function
    | Either.Left move -> Either.Left (Moves1.get_subject_name move)
    | Either.Right move -> Either.Right (Moves2.get_subject_name move)

  let get_namectx = function
    | Either.Left move ->
        (Moves1.get_namectx move, Moves2.Renaming.Namectx.empty)
    | Either.Right move ->
        (Moves1.Renaming.Namectx.empty, Moves2.get_namectx move)

  let fresh_names (weakening1, weakening2) = function
    | Either.Left move ->
        List.map (fun nn -> Either.Left nn) (Moves1.fresh_names weakening1 move)
    | Either.Right move ->
        List.map
          (fun nn -> Either.Right nn)
          (Moves2.fresh_names weakening2 move)

  (* A free name stays on its side. *)
  let map_free_names f = function
    | Either.Left move ->
        Either.Left
          (Moves1.map_free_names
             (fun nn ->
               match f (Either.Left nn) with
               | Either.Left nn' -> nn'
               | Either.Right _ ->
                   failwith "Renaming a free name to the other side.")
             move)
    | Either.Right move ->
        Either.Right
          (Moves2.map_free_names
             (fun nn ->
               match f (Either.Right nn) with
               | Either.Right nn' -> nn'
               | Either.Left _ ->
                   failwith "Renaming a free name to the other side.")
             move)

  let erase_display_hints = function
    | Either.Left move -> Either.Left (Moves1.erase_display_hints move)
    | Either.Right move -> Either.Right (Moves2.erase_display_hints move)

  let is_equiv_move move move' =
    match (move, move') with
    | (Either.Left move, Either.Left move') -> Moves1.is_equiv_move move move'
    | (Either.Right move, Either.Right move') ->
        Moves2.is_equiv_move move move'
    | _ -> false

  let is_equiv_pol_move (dir, move) (dir', move') =
    dir = dir' && is_equiv_move move move'
end
