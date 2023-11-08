(*TODO: Reimplement it using Moves:Moves.MOVES*)

module WBLTS
    (ContNames : Lang.Cps.CONT_NAMES)
    (Moves : Moves.MOVES with type name = ContNames.name) :
  Hislts.HISLTS_INIT with type move = Moves.move and type name = ContNames.name =
struct
  type move = Moves.move
  type active_conf = ContNames.cont_name list
  type passive_conf = ContNames.cont_name list

  let string_of_active_conf cstack =
    String.concat "::" (List.map ContNames.string_of_cont_name cstack)

  let string_of_passive_conf cstack =
    String.concat "::" (List.map ContNames.string_of_cont_name cstack)

  let p_trans cstack move =
    let support = Moves.get_transmitted_names move in
    let cstack' = List.filter_map ContNames.get_cont_name support in
    cstack' @ cstack

  let o_trans_check cstack move =
    let subject_names = Moves.get_subject_names move in
    let subject_cnames = List.filter_map ContNames.get_cont_name subject_names in
    match (subject_cnames, cstack) with
    | ([ cn ], cn' :: cstack') when cn = cn' ->
        Some cstack' (*We only deal with popping a single continuation name *)
    | ([], _) -> Some cstack
    | (_, _) -> None

  type name = ContNames.name

  let init_aconf _ = []
  let init_pconf _ _ = []
end
