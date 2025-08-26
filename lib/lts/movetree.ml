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

  module Storectx = struct
    type name = unit
    type t = unit
    type typ = unit

    let to_string _ = ""
    let pp _ _ = failwith ""
    let empty = ()
    let concat _ _ = ()
    let to_yojson = failwith ""
    let get_names _ = []
    let lookup_exn _ = failwith ""
    let is_empty () = true
    let is_singleton () () () = false
    let is_last () () () = None
    let add _ = failwith ""
    let to_pmap _ = failwith ""
    let mem () () = true
    let singleton () = ((), ())
    let add_fresh () _ () = ((), ())
    let map _ () = ()
  end

  let infer_type_store : store -> Storectx.t = failwith ""

  type opconf = MoveTree.Moves.move * store

  let string_of_opconf : opconf -> string = failwith ""
  let pp_opconf : Format.formatter -> opconf -> unit = failwith ""

  module Namectx = MoveTree.Moves.Namectx

  module IEnv =
    Lang.Ienv.Make_PMAP
      (MoveTree.Moves.Names)
      (struct
        type t = MoveTree.Moves.Names.name [@@deriving to_yojson]

        let pp = MoveTree.Moves.Names.pp_name
      end)

  
  type abstract_normal_form = MoveTree.Moves.move

  let eval ((move, movetree), namectx, storectx) :
      ((abstract_normal_form * Namectx.t * Storectx.t)
      * IEnv.t
      * store)
      EvalMonad.m =
    match MoveTree.trigger movetree move with
    | None -> EvalMonad.fail ()
    | Some moveOut ->
        EvalMonad.return ((moveOut, namectx, storectx), IEnv.empty, movetree)

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
      Storectx.t ->
      Namectx.t ->
      (abstract_normal_form * Namectx.t * Namectx.t) BranchMonad.m =
    (*MoveTree.Moves.generate_moves - Need to handle storectx*)
    failwith ""

  let type_check_a_nf :
      Namectx.t ->
      Namectx.t ->
      abstract_normal_form * Namectx.t ->
      Namectx.t option =
    failwith ""
  (* There's a mismatch with the signature of MoveTree.Move.check_type_move *)

  let concretize_a_nf movetree renaming move = ((move, movetree), renaming)
end
