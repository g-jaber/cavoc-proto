module type MOVES = sig
  (* to be instantiated *)
  module Renaming : Lang.Renaming.WEAKENING
  (* *)

  type copattern
  type move = Renaming.Namectx.Names.name * copattern [@@deriving to_yojson]

  val pp_move : Format.formatter -> move -> unit
  val string_of_move : move -> string

  (* The _in variants display the names of the ambient context with the
     provided show_name, typically Namectx.show_name_in of that context. *)
  val pp_move_in :
    show_name:(Renaming.Namectx.Names.name -> string) ->
    Format.formatter ->
    move ->
    unit

  val string_of_move_in :
    show_name:(Renaming.Namectx.Names.name -> string) -> move -> string

  val move_to_yojson_in :
    show_name:(Renaming.Namectx.Names.name -> string) -> move -> Yojson.Safe.t

  val get_subject_name : move -> Renaming.Namectx.Names.name
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

  val pp_pol_move_in :
    show_name:(Renaming.Namectx.Names.name -> string) ->
    Format.formatter ->
    pol_move ->
    unit

  val string_of_pol_move_in :
    show_name:(Renaming.Namectx.Names.name -> string) -> pol_move -> string

  val pol_move_to_yojson_in :
    show_name:(Renaming.Namectx.Names.name -> string) ->
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

  val pp_a_nf_in :
    pp_dir:(Format.formatter -> unit) ->
    pp_free_name:
      (Format.formatter -> IEnv.Renaming.Namectx.Names.name -> unit) ->
    pp_bound_name:
      (Format.formatter -> IEnv.Renaming.Namectx.Names.name -> unit) ->
    Format.formatter ->
    abstract_normal_form ->
    unit

  val string_of_a_nf : string -> abstract_normal_form -> string

  val get_subject_name :
    abstract_normal_form -> IEnv.Renaming.Namectx.Names.name

  val is_equiv_a_nf :
    IEnv.Renaming.Namectx.Names.name Util.Namespan.namespan ->
    abstract_normal_form ->
    abstract_normal_form ->
    IEnv.Renaming.Namectx.Names.name Util.Namespan.namespan option
end

module Make (A_nf : A_NF) :
  POLMOVES
    with module Renaming = A_nf.IEnv.Renaming
     and type copattern = A_nf.abstract_normal_form * A_nf.IEnv.Renaming.t =
struct
  module Renaming = A_nf.IEnv.Renaming

  type copattern = A_nf.abstract_normal_form * Renaming.t
  type move = Renaming.Namectx.Names.name * copattern
  type direction = Input | Output [@@deriving to_yojson]

  let string_of_direction = function Input -> "?" | Output -> "!"
  let switch = function Input -> Output | Output -> Input

  type pol_move = direction * move

  (* Bound names are displayed reindexed through the carried renaming;
     free names are resolved by show_name. *)
  let pp_move_gen ~show_name ~pp_dir fmt (_, (a_nf, renaming)) =
    let pp_free_name fmt nn = Format.pp_print_string fmt (show_name nn) in
    let pp_bound_name fmt nn =
      Renaming.Namectx.Names.pp_name fmt (Renaming.lookup renaming nn) in
    A_nf.pp_a_nf_in ~pp_dir ~pp_free_name ~pp_bound_name fmt a_nf

  let default_show = Renaming.Namectx.Names.string_of_name

  let pp_move_in ~show_name fmt move =
    let pp_dir fmt = Format.pp_print_string fmt "" in
    pp_move_gen ~show_name ~pp_dir fmt move

  let pp_move = pp_move_in ~show_name:default_show

  let string_of_move_in ~show_name =
    Format.asprintf "%a" (pp_move_in ~show_name)

  let string_of_move = string_of_move_in ~show_name:default_show

  let pp_pol_move_in ~show_name fmt (dir, move) =
    let pp_dir fmt = Format.pp_print_string fmt (string_of_direction dir) in
    pp_move_gen ~show_name ~pp_dir fmt move

  let pp_pol_move = pp_pol_move_in ~show_name:default_show

  let string_of_pol_move_in ~show_name =
    Format.asprintf "%a" (pp_pol_move_in ~show_name)

  let string_of_pol_move = string_of_pol_move_in ~show_name:default_show

  let move_to_yojson_in ~show_name ((_, (a_nf, _)) as move : move) =
    `Assoc
      [
        ("subjectName", `String (show_name (A_nf.get_subject_name a_nf)));
        ("string", `String (string_of_move_in ~show_name move));
      ]

  let move_to_yojson = move_to_yojson_in ~show_name:default_show

  let pol_move_to_yojson_in ~show_name (_, move) =
    move_to_yojson_in ~show_name move

  let pol_move_to_yojson (_, move) = move_to_yojson move

  let yojson_of_move (m : move) : Yojson.Safe.t =
    `Assoc [ ("label", `String (string_of_move m)) ]

  let switch_direction (p, d) = (switch p, d)
  let get_subject_name (nn, (_, _)) = nn
  let get_namectx (_, (_, renaming)) = Renaming.dom renaming

  let unify_move span (_, (a_nf1, _)) (_, (a_nf2, _)) =
    A_nf.is_equiv_a_nf span a_nf1 a_nf2

  let unify_pol_move span (dir1, move1) (dir2, move2) =
    if dir1 = dir2 then unify_move span move1 move2 else None
end
