module type MOVES = sig
  (* to be instantiated *)
  module Names : Lang.Names.NAMES
  (* *)

  type move

  val pp_move : Format.formatter -> move -> unit
  val string_of_move : move -> string
  val get_subject_name : move -> Names.name list
  val get_transmitted_names : move -> Names.name list

  val unify_move :
    Names.name Util.Namespan.namespan ->
    move ->
    move ->
    Names.name Util.Namespan.namespan option
end

module type POLMOVES = sig
  include MOVES

  type direction = Input | Output
  type pol_move = direction * move

  val pp_pol_move : Format.formatter -> pol_move -> unit
  val string_of_pol_move : pol_move -> string
  val switch_direction : pol_move -> pol_move

  val unify_pol_move :
    Names.name Util.Namespan.namespan ->
    pol_move ->
    pol_move ->
    Names.name Util.Namespan.namespan option
end

module type TYPED_MOVES = sig
  include MOVES

  module Namectx : Lang.Namectx.NAMECTX with type name = Names.name

  module BranchMonad : Util.Monad.BRANCH

  val generate_moves : Namectx.name_ctx -> (move * Namectx.name_ctx) BranchMonad.m
  val infer_type_move : Namectx.name_ctx -> move -> Namectx.name_ctx option
  val check_type_move : Namectx.name_ctx -> move * Namectx.name_ctx -> bool
end

module type NAMED_TYPED_MOVES = sig
  type copattern
  type name

  include TYPED_MOVES with type move = name * copattern and type Names.name = name
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

module type A_NF = sig
  module Names : Lang.Names.NAMES

  type abstract_normal_form

  val pp_a_nf :
    pp_dir:(Format.formatter -> unit) ->
    Format.formatter ->
    abstract_normal_form ->
    unit

  val string_of_a_nf : string -> abstract_normal_form -> string
  val get_subject_name : abstract_normal_form -> Names.name option
  val get_support : abstract_normal_form -> Names.name list

  val is_equiv_a_nf :
    Names.name Util.Namespan.namespan ->
    abstract_normal_form ->
    abstract_normal_form ->
    Names.name Util.Namespan.namespan option
end

module Make (A_nf : A_NF) :
  POLMOVES
    with module Names = A_nf.Names
     and type move = A_nf.abstract_normal_form = struct
  module Names = A_nf.Names

  type move = A_nf.abstract_normal_form
  type direction = Input | Output

  let string_of_direction = function Input -> "?" | Output -> "!"
  let switch = function Input -> Output | Output -> Input

  type pol_move = direction * move

  let pp_move fmt =
    let pp_dir fmt = Format.pp_print_string fmt "" in
    A_nf.pp_a_nf ~pp_dir fmt

  let pp_pol_move fmt (dir, move) =
    let pp_dir fmt = Format.pp_print_string fmt (string_of_direction dir) in
    A_nf.pp_a_nf ~pp_dir fmt move

  let string_of_move = A_nf.string_of_a_nf ""

  let string_of_pol_move (dir, move) =
    A_nf.string_of_a_nf (string_of_direction dir) move

  let switch_direction (p, d) = (switch p, d)
  let get_transmitted_names = A_nf.get_support

  let get_subject_name move =
    match A_nf.get_subject_name move with Some n -> [ n ] | None -> []

  let unify_move = A_nf.is_equiv_a_nf

  let unify_pol_move span (dir1, move1) (dir2, move2) =
    if dir1 = dir2 then unify_move span move1 move2 else None
end
