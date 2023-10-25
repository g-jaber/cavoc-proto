module Make (IntLts:Bilts.INT_LTS) 
            (HistLts:Hislts.HISLTS_INIT  with type move = IntLts.move and type name = IntLts.Int.Actions.Lang.name) 
        : Bilts.INT_LTS with module Int = IntLts.Int  
  = struct

  module M=IntLts.M
  open M
  (* *)
  module Int = IntLts.Int
  type action = IntLts.action 
  type move = IntLts.move
  let get_move_from_action = IntLts.get_move_from_action

  type active_conf = IntLts.active_conf * HistLts.active_conf

  type passive_conf = IntLts.passive_conf * HistLts.passive_conf
  type conf = 
  | Active of active_conf
  | Passive of passive_conf

  let string_of_active_conf (iconf,hconf) = 
    (IntLts.string_of_active_conf iconf) ^ ";" ^ (HistLts.string_of_active_conf hconf)

  let string_of_passive_conf (iconf,hconf) = 
    (IntLts.string_of_passive_conf iconf) ^ ";" ^ (HistLts.string_of_passive_conf hconf)

  let p_trans (active_iconf,active_hconf) =
    let (action,passive_iconf_option) = IntLts.p_trans active_iconf in
    match (IntLts.get_move_from_action action,passive_iconf_option) with
      | (Some output_move,Some passive_iconf) -> 
        let passive_hconf = HistLts.p_trans active_hconf output_move in
        (action,Some (passive_iconf,passive_hconf))
      | (None,None) -> (action,None)
      | _ -> failwith "Impossible branch"
  let equiv_aconf _ _ = failwith "Not yet implemented"
  
  let o_trans (passive_iconf,passive_hconf) =
    let* (action,active_iconf) = IntLts.o_trans passive_iconf in
    match IntLts.get_move_from_action action with
      | None -> failwith "Opponent is performing an action that is not a move. Please report."
      | Some input_move -> begin
         match HistLts.o_trans_check passive_hconf input_move with
            | None -> fail ()
            | Some active_hconf -> return (action,(active_iconf,active_hconf))
        end

    let init_aconf comp namectxP = 
      let init_iconf = IntLts.init_aconf comp namectxP in
      let init_hconf = HistLts.init_aconf (Util.Pmap.dom namectxP) in
      (init_iconf,init_hconf)
    let init_pconf ienv namectxP namectxO =
      let init_iconf = IntLts.init_pconf ienv namectxP namectxO in
      let init_hconf = HistLts.init_pconf (Util.Pmap.dom namectxP) (Util.Pmap.dom namectxO) in
      (init_iconf,init_hconf)
end
