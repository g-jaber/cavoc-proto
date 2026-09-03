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

  val unify_move :
    Renaming.Namectx.Names.name Util.Namespan.namespan ->
    move ->
    move ->
    Renaming.Namectx.Names.name Util.Namespan.namespan option
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

  val unify_pol_move :
    Renaming.Namectx.Names.name Util.Namespan.namespan ->
    pol_move ->
    pol_move ->
    Renaming.Namectx.Names.name Util.Namespan.namespan option
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

  val unify_pol_move :
end *)

module Make (A_nf : Lang.Interactive.A_NF) :
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

  let unify_move span (a_nf1, _) (a_nf2, _) =
    A_nf.is_equiv_a_nf span a_nf1 a_nf2

  let unify_pol_move span (dir1, move1) (dir2, move2) =
    if dir1 = dir2 then unify_move span move1 move2 else None
end
