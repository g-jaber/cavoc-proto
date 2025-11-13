module type MOVES = sig
  (* to be instantiated *)
  module Renaming : Lang.Renaming.RENAMING
  (* *)

  type move [@@deriving to_yojson]

  val pp_move : Format.formatter -> move -> unit
  val string_of_move : move -> string
  val get_subject_name : move -> Renaming.Namectx.Names.name option
  val get_namectx : move -> Renaming.Namectx.t

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

module type NAMED_GEN_MOVES = sig
  type copattern
  type name

  include
    GEN_MOVES
      with type move = name * copattern
       and type Renaming.Namectx.Names.name = name
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

  type abstract_normal_form [@@deriving to_yojson]

  val renaming_a_nf :
    IEnv.Renaming.t -> abstract_normal_form -> abstract_normal_form

  val pp_a_nf :
    pp_dir:(Format.formatter -> unit) ->
    Format.formatter ->
    abstract_normal_form ->
    unit

  val string_of_a_nf : string -> abstract_normal_form -> string

  val get_subject_name :
    abstract_normal_form -> IEnv.Renaming.Namectx.Names.name option

  val is_equiv_a_nf :
    IEnv.Renaming.Namectx.Names.name Util.Namespan.namespan ->
    abstract_normal_form ->
    abstract_normal_form ->
    IEnv.Renaming.Namectx.Names.name Util.Namespan.namespan option
end

module Make (A_nf : A_NF) :
  POLMOVES
    with module Renaming = A_nf.IEnv.Renaming
     and type move = A_nf.abstract_normal_form * A_nf.IEnv.Renaming.t = struct
  module Renaming = A_nf.IEnv.Renaming

  type move = A_nf.abstract_normal_form * Renaming.t
  type direction = Input | Output [@@deriving to_yojson]

  let move_to_yojson ((a_nf,_):move) : Yojson.Safe.t = 
    A_nf.abstract_normal_form_to_yojson a_nf



  let string_of_direction = function Input -> "?" | Output -> "!"
  let switch = function Input -> Output | Output -> Input

  type pol_move = direction * move [@@deriving to_yojson]

  (* We always rename moves *)
  let pp_move fmt (move, renaming) =
    let move' = A_nf.renaming_a_nf renaming move in
    let pp_dir fmt = Format.pp_print_string fmt "" in
    A_nf.pp_a_nf ~pp_dir fmt move'

  let pp_pol_move fmt (dir, (move, renaming)) =
    let move' = A_nf.renaming_a_nf renaming move in
    let pp_dir fmt = Format.pp_print_string fmt (string_of_direction dir) in
    Format.fprintf fmt "%a" (A_nf.pp_a_nf ~pp_dir) move'
      (* (was %a) (A_nf.pp_a_nf ~pp_dir) move*)

  let string_of_move move = Format.asprintf "%a" pp_move move
  let string_of_pol_move polmove = Format.asprintf "%a" pp_pol_move polmove

  let yojson_of_move (m : move) : Yojson.Safe.t =
  `Assoc [ ("label", `String (string_of_move m)) ]

  let switch_direction (p, d) = (switch p, d)
  let get_subject_name (move, _) = A_nf.get_subject_name move
  let get_namectx (_, renaming) = Renaming.dom renaming

  let unify_move span (a_nf1, _) (a_nf2, _) =
    A_nf.is_equiv_a_nf span a_nf1 a_nf2

  let unify_pol_move span (dir1, move1) (dir2, move2) =
    if dir1 = dir2 then unify_move span move1 move2 else None
end
