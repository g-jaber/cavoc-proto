module Make
    (TypingLTS : Typing.LTS)
    (HistLts :
      Hislts.HISLTS_INIT
        with type move = TypingLTS.Moves.pol_move
         and type name = TypingLTS.Moves.Renaming.Namectx.Names.name) :
  Typing.LTS
    with module Moves = TypingLTS.Moves
     and type store_ctx = TypingLTS.store_ctx = struct
  module Moves = TypingLTS.Moves
  module BranchMonad = TypingLTS.BranchMonad

  type store_ctx = TypingLTS.store_ctx

  (* *)

  type position = TypingLTS.position * HistLts.conf [@@deriving to_yojson]

  let pp_position fmt (pos, hconf) =
    Format.fprintf fmt "@[%a |@, %a@]" TypingLTS.pp_position pos HistLts.pp_conf
      hconf

  let string_of_position = Format.asprintf "%a" pp_position
  let get_namectxO (pos, _) = TypingLTS.get_namectxO pos
  let get_storectx (pos, _) = TypingLTS.get_storectx pos

  let generate_moves (pos, hconf) =
    let open BranchMonad in
    let* (move, pos') = TypingLTS.generate_moves pos in
    match HistLts.trans_check hconf move with
    | None -> fail ()
    | Some hconf' -> return (move, (pos', hconf'))

  (* check_move Γₓ m return Some Δ
     when there exists a name context Γ for the free names of m such that
      Γₓ ⊢ m ▷ Δ.
     It returns None when m is not well-typed.*)
  let check_move (pos, hconf) move =
    match (TypingLTS.check_move pos move, HistLts.trans_check hconf move) with
    | (None, _) | (_, None) -> None
    | (Some pos', Some hconf') -> Some (pos', hconf')

  let trigger_move (pos, hconf) move =
    let pos' = TypingLTS.trigger_move pos move in
    match HistLts.trans_check hconf move with
    | None -> failwith "TODO"
    | Some hconf' -> (pos', hconf')

  let init_act_pos storectx namectxP namectxO =
    let pos = TypingLTS.init_act_pos storectx namectxP namectxO in
    let namesP = TypingLTS.Moves.Renaming.Namectx.get_names namectxP in
    let namesO = TypingLTS.Moves.Renaming.Namectx.get_names namectxO in
    let hconf = HistLts.init_act_conf namesP namesO in
    (pos, hconf)

  let init_pas_pos storectx namectxP namectxO =
    let pos = TypingLTS.init_pas_pos storectx namectxP namectxO in
    let namesP = TypingLTS.Moves.Renaming.Namectx.get_names namectxP in
    let namesO = TypingLTS.Moves.Renaming.Namectx.get_names namectxO in
    let hconf = HistLts.init_pas_conf namesP namesO in
    (pos, hconf)
end
