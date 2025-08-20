module type MOVETREE = sig
  module Moves : Moves.NAMED_TYPED_MOVES

  type movetree = {
    root: Moves.name;
    namectxP: Moves.Namectx.t;
    namectxO: Moves.Namectx.t;
    map: (Moves.move, Moves.move) Util.Pmap.pmap;
  }

  val trigger : movetree -> Moves.move -> Moves.move option
  val update : movetree -> Moves.move * Moves.move -> movetree option
end

module Make (Moves : Moves.NAMED_TYPED_MOVES) : MOVETREE = struct
  module Moves = Moves

  type movetree = {
    root: Moves.name;
    namectxP: Moves.Namectx.t;
    namectxO: Moves.Namectx.t;
    map: (Moves.move, Moves.move) Util.Pmap.pmap;
  }

  let trigger movetree move = Util.Pmap.lookup move movetree.map

  let update movetree (moveIn, moveOut) =
    match Util.Pmap.lookup moveIn movetree.map with
    | None ->
        let map = Util.Pmap.add (moveIn, moveOut) movetree.map in
        Some { movetree with map }
    | Some moveOut' -> (
        match Moves.unify_move Util.Namespan.empty_nspan moveOut moveOut' with
        | None -> None
        | Some _ -> Some movetree)
end

module MakeLang (MoveTree : MOVETREE) : Lang.Interactive.LANG = struct
  module Names = MoveTree.Moves.Names
  module EvalMonad = Util.Monad.Option
  module BranchMonad = MoveTree.Moves.BranchMonad

  type store = MoveTree.movetree

  let string_of_store : store -> string = failwith ""
  let pp_store : Format.formatter -> store -> unit = failwith ""

  type store_ctx = unit

  let string_of_store_ctx : store_ctx -> string = failwith ""
  let pp_store_ctx : Format.formatter -> store_ctx -> unit = failwith ""
  let empty_store_ctx : store_ctx = failwith ""
  let infer_type_store : store -> store_ctx = failwith ""

  type opconf = MoveTree.Moves.move * store

  let string_of_opconf : opconf -> string = failwith ""
  let pp_opconf : Format.formatter -> opconf -> unit = failwith ""

  module Namectx = MoveTree.Moves.Namectx

  type interactive_env =
    (MoveTree.Moves.name, MoveTree.Moves.name) Util.Pmap.pmap

  let interactive_env_to_yojson = failwith ""
  let empty_ienv : interactive_env = Util.Pmap.empty

  let concat_ienv : interactive_env -> interactive_env -> interactive_env =
    Util.Pmap.concat

  let pp_ienv : Format.formatter -> interactive_env -> unit = failwith ""
  let string_of_ienv : interactive_env -> string = failwith ""

  type abstract_normal_form = MoveTree.Moves.move

  let eval ((move, movetree), namectx, storectx) :
      ((abstract_normal_form * Namectx.t * store_ctx) * interactive_env * store)
      EvalMonad.m =
    match MoveTree.trigger movetree move with
    | None -> EvalMonad.fail ()
    | Some moveOut ->
        EvalMonad.return ((moveOut, namectx, storectx), empty_ienv, movetree)

  let get_subject_name : abstract_normal_form -> Names.name option = failwith ""
  let get_support : abstract_normal_form -> Names.name list = failwith ""

  let pp_a_nf :
      pp_dir:(Format.formatter -> unit) ->
      Format.formatter ->
      abstract_normal_form ->
      unit =
    failwith "" (*MoveTree.Moves.pp_move - Need to handle pp_dir *)

  let string_of_a_nf : string -> abstract_normal_form -> string = failwith ""

  let is_equiv_a_nf :
      Names.name Util.Namespan.namespan ->
      abstract_normal_form ->
      abstract_normal_form ->
      Names.name Util.Namespan.namespan option =
    MoveTree.Moves.unify_move

  let generate_a_nf :
      store_ctx ->
      Namectx.t ->
      (abstract_normal_form * Namectx.t * Namectx.t) BranchMonad.m =
    (*MoveTree.Moves.generate_moves - Need to handle storectx*)
    failwith ""

  let type_check_a_nf :
      Namectx.t ->
      Namectx.t ->
      abstract_normal_form ->
      (Namectx.t * Namectx.t) option =
    failwith ""
  (* There's a mismatch with the signature of MoveTree.Move.check_type_move *)

  let concretize_a_nf movetree renaming move = ((move, movetree), renaming)
end
