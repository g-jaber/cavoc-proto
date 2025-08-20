module type MOVETREE = sig
  module Moves : Moves.NAMED_TYPED_MOVES

  type movetree = {
    root: Moves.name;
    namectxP: Moves.Namectx.name_ctx;
    namectxO: Moves.Namectx.name_ctx;
    map: (Moves.move, Moves.move) Util.Pmap.pmap;
  }
  val trigger : movetree -> Moves.move -> Moves.move option
  val update : movetree -> Moves.move * Moves.move -> movetree option
end

module Make (Moves : Moves.NAMED_TYPED_MOVES) : MOVETREE = struct
  module Moves = Moves

  type movetree = {
    root: Moves.name;
    namectxP: Moves.Namectx.name_ctx;
    namectxO: Moves.Namectx.name_ctx;
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

(*
module MakeLang (MoveTree:MOVETREE) : Lang.Interactive.LANG = struct

end
*)
