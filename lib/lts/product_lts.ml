module Make
    (GameLTS : Gamelts.GAME)
    (HistLts :
      Hislts.HISLTS_INIT
        with type move = GameLTS.Moves.move) :
  Gamelts.GAME with module Moves = GameLTS.Moves = struct
  module Moves = GameLTS.Moves
  module BranchMonad = GameLTS.BranchMonad
  type namectx = GameLTS.name_ctx
  type storectx = GameLTS.store_ctx

  (* *)

  type position = GameLTS.position * HistLts.active_conf [@@deriving to_yojson]

  type active_conf = GameLTS.active_conf * HistLts.active_conf

  type passive_conf = GameLTS.passive_conf * HistLts.passive_conf
  [@@deriving to_yojson]

  type conf = Active of active_conf | Passive of passive_conf

  let pp_active_conf fmt (iconf, hconf) =
    Format.fprintf fmt "@[%a |@, %a@]" GameLTS.pp_active_conf iconf
      HistLts.pp_active_conf hconf

  let pp_passive_conf fmt (iconf, hconf) =
    Format.fprintf fmt "@[%a |@, %a@]" GameLTS.pp_passive_conf iconf
      HistLts.pp_passive_conf hconf

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf
  let equiv_act_conf _ _ = failwith "Not yet implemented"

  let p_trans (active_iconf, active_hconf) =
    let open EvalMonad in
    let* (output_move, passive_iconf) = GameLTS.p_trans active_iconf in
    let passive_hconf = HistLts.p_trans active_hconf output_move in
    return (output_move, (passive_iconf, passive_hconf))

  let o_trans (passive_iconf, passive_hconf) input_move =
    match
      ( GameLTS.o_trans passive_iconf input_move,
        HistLts.o_trans_check passive_hconf input_move )
    with
    | (None, _) | (_, None) -> None
    | (Some active_iconf, Some active_hconf) -> Some (active_iconf, active_hconf)

  let o_trans_gen (passive_iconf, passive_hconf) =
    let open BranchMonad in
    let* (input_move, active_iconf) = GameLTS.o_trans_gen passive_iconf in
    match HistLts.o_trans_check passive_hconf input_move with
    | None -> fail ()
    | Some active_hconf -> return (input_move, (active_iconf, active_hconf))

  let init_aconf comp namectxP =
    let init_iconf = GameLTS.init_aconf comp namectxP in
    let init_hconf =
      HistLts.init_aconf (GameLTS.Int.IntLang.get_names_from_name_ctx namectxP)
    in
    (init_iconf, init_hconf)

  let init_pconf store ienv namectxP namectxO =
    let init_iconf = GameLTS.init_pconf store ienv namectxP namectxO in
    let init_hconf =
      HistLts.init_pconf
        (GameLTS.Int.IntLang.get_names_from_name_ctx namectxP)
        (GameLTS.Int.IntLang.get_names_from_name_ctx namectxO) in
    (init_iconf, init_hconf)
end
