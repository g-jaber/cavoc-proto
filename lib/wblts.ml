(*TODO: Reimplement it using Moves:Moves.MOVES*)

module WBLTS (Moves:Cps.MOVES) : (Hislts.HISLTS_INIT with type move = Moves.move and type name = Moves.ContNames.name) =
  struct
  type move = Moves.move
  type active_conf = Moves.ContNames.cont_name list
  type passive_conf = Moves.ContNames.cont_name list
  let string_of_active_conf cstack = String.concat "::" (List.map Moves.ContNames.string_of_cont_name cstack)
  let string_of_passive_conf cstack = String.concat "::" (List.map Moves.ContNames.string_of_cont_name cstack)
  let p_trans  cstack move =
    let cstack' = Moves.get_transmitted_continuation_names move 
    in cstack'@cstack
  let o_trans_check cstack move =
    match (Moves.get_active_continuation_name move,cstack) with
      | (Some cn,cn'::cstack') when cn = cn' -> Some cstack'
      | (Some _,_) -> None
      | (None,_) -> Some cstack 

  type name = Moves.ContNames.name
  let init_aconf _ = []
  let init_pconf _ _ = []
end