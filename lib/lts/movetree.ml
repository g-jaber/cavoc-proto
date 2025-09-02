module type MOVETREE = sig
  module Moves : Moves.NAMED_GEN_MOVES

  type movetree = {
    root: Moves.Renaming.Namectx.Names.name;
    namectxP: Moves.Renaming.Namectx.t;
    namectxO: Moves.Renaming.Namectx.t;
    map: (Moves.move, Moves.move) Util.Pmap.pmap;
  }

  val pp : Format.formatter -> movetree -> unit
  val trigger : movetree -> Moves.move -> Moves.move option
  val update : movetree -> Moves.move * Moves.move -> movetree option
end

module Make (Moves : Moves.NAMED_GEN_MOVES) : MOVETREE = struct
  module Moves = Moves

  type movetree = {
    root: Moves.Renaming.Namectx.Names.name;
    namectxP: Moves.Renaming.Namectx.t;
    namectxO: Moves.Renaming.Namectx.t;
    map: (Moves.move, Moves.move) Util.Pmap.pmap;
  }

  let pp fmt movetree =
    let pp_sep fmt () = Format.fprintf fmt ", " in
    let pp_empty fmt () = Format.fprintf fmt "⋅" in
    let pp_pair fmt (m, m') =
      Format.fprintf fmt "%a : %a" Moves.pp_move m Moves.pp_move m' in
    Util.Pmap.pp_pmap ~pp_empty ~pp_sep pp_pair fmt movetree.map

  let trigger movetree move = Util.Pmap.lookup move movetree.map

  let update movetree (moveIn, moveOut) =
    match Util.Pmap.lookup moveIn movetree.map with
    | None ->
        let map = Util.Pmap.add (moveIn, moveOut) movetree.map in
        Some { movetree with map }
    | Some moveOut' -> (
        match Moves.unify_move Util.Namespan.empty_nspan moveOut moveOut' with
        (* We need unify only if there are some disclosed locations*)
        | None -> None
        | Some _ -> Some movetree)
end

module MakeLang (MoveTree : MOVETREE with type Moves.name = int * string) :
  Lang.Interactive.LANG = struct
  module Namectx = MoveTree.Moves.Renaming.Namectx
  module Names = Namectx.Names
  module EvalMonad = Util.Monad.Option
  module BranchMonad = MoveTree.Moves.BranchMonad

  type store = MoveTree.movetree

  let pp_store = MoveTree.pp
  let string_of_store = Format.asprintf "%a" pp_store

  module Storectx = Namectx

  let infer_type_store movetree =
    let open MoveTree in
    movetree.namectxP

  type opconf = MoveTree.Moves.move * store

  let pp_opconf fmt (move, movetree) =
    Format.fprintf fmt "⟨%a | %a ⟩" MoveTree.Moves.pp_move move MoveTree.pp
      movetree

  let string_of_opconf = Format.asprintf "%a" pp_opconf

  module Renaming = MoveTree.Moves.Renaming

  module IEnv =
    (* We could use explicitely a renaming here *)
      Lang.Ienv.Make_List
        (Renaming)
        (struct
          type t = Names.name [@@deriving to_yojson]

          let embed_name = Fun.id
          let renam_act renam1 nn = Renaming.lookup renam1 nn
          let pp = Names.pp_name
        end)

  type abstract_normal_form = MoveTree.Moves.move

  let renaming_a_nf _renaming = failwith "TODO"

  let eval ((move, movetree), namectx, storectx) :
      ((abstract_normal_form * Namectx.t * Storectx.t) * IEnv.t * store)
      EvalMonad.m =
    match MoveTree.trigger movetree move with
    | None -> EvalMonad.fail ()
    | Some moveOut ->
        EvalMonad.return
          ((moveOut, namectx, storectx), IEnv.empty namectx, movetree)

  let get_subject_name : abstract_normal_form -> Names.name option =
    MoveTree.Moves.get_subject_name

  let[@warning "-27"] pp_a_nf ~pp_dir fmt =
    MoveTree.Moves.pp_move
      fmt (*MoveTree.Moves.pp_move - Need to handle pp_dir *)

  let string_of_a_nf dir =
    let pp_dir fmt = Format.fprintf fmt "%s" dir in
    Format.asprintf "%a" (pp_a_nf ~pp_dir)

  let is_equiv_a_nf :
      Names.name Util.Namespan.namespan ->
      abstract_normal_form ->
      abstract_normal_form ->
      Names.name Util.Namespan.namespan option =
    MoveTree.Moves.unify_move

  let generate_a_nf _storectx namectx :
      (abstract_normal_form * Namectx.t * Namectx.t) BranchMonad.m =
      let open BranchMonad in
      let* (move,lnamectx) = MoveTree.Moves.generate_moves namectx in
      return (move,lnamectx,namectx) (*Need to handle storectx*)

  let type_check_a_nf namectxP _namectxO (a_nf,lnamectx) :
      Namectx.t option =
      if MoveTree.Moves.check_type_move namectxP (a_nf,lnamectx) then
        Some lnamectx
      else None

  let concretize_a_nf (movetree : store) (renaming : IEnv.t)
      ((a_nf, _renaming') : abstract_normal_form * Renaming.t) =
    ((a_nf, movetree), renaming)
end
