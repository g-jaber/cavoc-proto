module Make
    (TypingLTS : Typing.LTS)
    (HistLts :
      Hislts.HISLTS_INIT
        with type move = TypingLTS.Moves.pol_move
         and type renaming = TypingLTS.Moves.Renaming.t
         and type name = TypingLTS.Moves.Renaming.Namectx.Names.name) :
  Typing.LTS
    with module Moves = TypingLTS.Moves
     and type store_ctx = TypingLTS.store_ctx = struct
  module Moves = TypingLTS.Moves
  module BranchMonad = TypingLTS.BranchMonad

  type store_ctx = TypingLTS.store_ctx
  type position = TypingLTS.position * HistLts.conf [@@deriving to_yojson]

  let pp_position fmt (pos, hconf) =
    Format.fprintf fmt "@[%a |@, %a@]" TypingLTS.pp_position pos HistLts.pp_conf
      hconf

  let string_of_position = Format.asprintf "%a" pp_position
  let get_namectxO (pos, _) = TypingLTS.get_namectxO pos
  let get_namectxP (pos, _) = TypingLTS.get_namectxP pos
  let get_storectx (pos, _) = TypingLTS.get_storectx pos

  let generate_moves (pos, hconf) =
    let open BranchMonad in
    let* (move, weakening, pos') = TypingLTS.generate_moves pos in
    match HistLts.trans_check hconf weakening move with
    | None -> fail ()
    | Some hconf' -> return (move, weakening, (pos', hconf'))

  let check_move (pos, hconf) move =
    match TypingLTS.check_move pos move with
    | None -> None
    | Some (weakening, pos') ->
        Option.map
          (fun hconf' -> (weakening, (pos', hconf')))
          (HistLts.trans_check hconf weakening move)

  let trigger_move (pos, hconf) move =
    let (weakening, pos') = TypingLTS.trigger_move pos move in
    match HistLts.trans_check hconf weakening move with
    | None -> failwith "TODO"
    | Some hconf' -> (weakening, (pos', hconf'))

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
