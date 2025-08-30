module type MOVES = sig
  (* to be instantiated *)
  module Namectx : Lang.Typectx.TYPECTX
  (* *)

  type move

  val pp_move : Format.formatter -> move -> unit
  val string_of_move : move -> string
  val get_subject_name : move -> Namectx.Names.name list
  val get_namectx : move -> Namectx.t

  val unify_move :
    Namectx.Names.name Util.Namespan.namespan ->
    move ->
    move ->
    Namectx.Names.name Util.Namespan.namespan option
end

module type POLMOVES = sig
  include MOVES

  type direction = Input | Output
  type pol_move = direction * move

  val pp_pol_move : Format.formatter -> pol_move -> unit
  val string_of_pol_move : pol_move -> string
  val switch_direction : pol_move -> pol_move

  val unify_pol_move :
    Namectx.Names.name Util.Namespan.namespan ->
    pol_move ->
    pol_move ->
    Namectx.Names.name Util.Namespan.namespan option
end

module type GEN_POLMOVES = sig
  include POLMOVES
  module BranchMonad : Util.Monad.BRANCH

  val generate_moves : Namectx.t -> (move * Namectx.t) BranchMonad.m
  val infer_type_move : Namectx.t -> move -> Namectx.t option
  val check_type_move : Namectx.t -> move * Namectx.t -> bool
end

module type GEN_MOVES = sig
  include MOVES
  module BranchMonad : Util.Monad.BRANCH

  val generate_moves : Namectx.t -> (move * Namectx.t) BranchMonad.m
  val infer_type_move : Namectx.t -> move -> Namectx.t option
  val check_type_move : Namectx.t -> move * Namectx.t -> bool
end

module type NAMED_GEN_MOVES = sig
  type copattern
  type name

  include
    GEN_MOVES
      with type move = name * copattern
       and type Namectx.Names.name = name
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
  module IEnv : Lang.Ienv.IENV

  type abstract_normal_form

  val pp_a_nf :
    pp_dir:(Format.formatter -> unit) ->
    Format.formatter ->
    abstract_normal_form ->
    unit

  val string_of_a_nf : string -> abstract_normal_form -> string
  val get_subject_name : abstract_normal_form -> IEnv.Renaming.Namectx.Names.name option
  val get_support : abstract_normal_form -> IEnv.Renaming.Namectx.Names.name list

  val is_equiv_a_nf :
    IEnv.Renaming.Namectx.Names.name Util.Namespan.namespan ->
    abstract_normal_form ->
    abstract_normal_form ->
    IEnv.Renaming.Namectx.Names.name Util.Namespan.namespan option
end

module Make (A_nf : A_NF) :
  POLMOVES
    with module Namectx = A_nf.IEnv.Renaming.Namectx
     and type move = A_nf.abstract_normal_form * A_nf.IEnv.Renaming.Namectx.t = struct
  module Namectx = A_nf.IEnv.Renaming.Namectx

  type move = A_nf.abstract_normal_form * Namectx.t
  type direction = Input | Output

  let string_of_direction = function Input -> "?" | Output -> "!"
  let switch = function Input -> Output | Output -> Input

  type pol_move = direction * move

  let pp_move fmt (move, _) =
    let pp_dir fmt = Format.pp_print_string fmt "" in
    A_nf.pp_a_nf ~pp_dir fmt move

  let pp_pol_move fmt (dir, (move, _)) =
    let pp_dir fmt = Format.pp_print_string fmt (string_of_direction dir) in
    A_nf.pp_a_nf ~pp_dir fmt move

  let string_of_move (move, _) = A_nf.string_of_a_nf "" move

  let string_of_pol_move (dir, (move, _)) =
    A_nf.string_of_a_nf (string_of_direction dir) move

  let switch_direction (p, d) = (switch p, d)

  let get_subject_name (move, _) =
    match A_nf.get_subject_name move with Some n -> [ n ] | None -> []

  let get_namectx (_, lnamectx) = lnamectx

  let unify_move span (a_nf1, _) (a_nf2, _) =
    A_nf.is_equiv_a_nf span a_nf1 a_nf2

  let unify_pol_move span (dir1, move1) (dir2, move2) =
    if dir1 = dir2 then unify_move span move1 move2 else None
end
