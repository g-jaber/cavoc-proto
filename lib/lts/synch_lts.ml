module type INT_LTS = sig
  module Int : Interactive.INT
  include Bipartite.LTS with module Actions = Int.Actions

  (* init_aconf creates an configuration from two computations and a two name context for Opponent.
     Its memory, interactive env, and name context for Proponent are all set to empty*)
  val init_aconf :
    Int.IntLang.computation ->
    Int.IntLang.name_ctx ->
    Int.IntLang.computation ->
    Int.IntLang.name_ctx ->
    active_conf
end

module Make (IntLts : Bipartite.INT_LTS) :
  INT_LTS with module Int = IntLts.Int = struct
  module M = IntLts.M
  open M

  (* *)
  module Int = IntLts.Int
  module Actions = IntLts.Actions

  type active_conf =
    IntLts.active_conf
    * IntLts.active_conf
    * IntLts.Int.Name.name Util.Namespan.namespan

  type passive_conf =
    IntLts.passive_conf
    * IntLts.passive_conf
    * IntLts.Int.Name.name Util.Namespan.namespan

  type conf = Active of active_conf | Passive of passive_conf

  let string_of_active_conf (act_conf1, act_conf2, namespan) =
    IntLts.string_of_active_conf act_conf1
    ^ "|"
    ^ IntLts.string_of_active_conf act_conf2
    ^ "|"
    ^ Util.Namespan.string_of_span IntLts.Int.Name.string_of_name namespan

  let string_of_passive_conf (pas_conf1, pas_conf2, namespan) =
    IntLts.string_of_passive_conf pas_conf1
    ^ "|"
    ^ IntLts.string_of_passive_conf pas_conf2
    ^ "|"
    ^ Util.Namespan.string_of_span IntLts.Int.Name.string_of_name namespan

  let equiv_aconf _ _ = false (*failwith "Not yet implemented"*)

  let p_trans (act_conf1, act_conf2, span) =
    let (action1, pas_conf_opt1) = IntLts.p_trans act_conf1 in
    let (action2, pas_conf_opt2) = IntLts.p_trans act_conf2 in
    match (pas_conf_opt1, pas_conf_opt2) with
    | (None, _) | (_, None) -> (Actions.error_action, None)
    | (Some pas_conf1, Some pas_conf2) -> begin
        match Actions.unify_action span action1 action2 with
        | None ->
            Util.Debug.print_debug @@ "Cannot synchronize output actions "
            ^ Actions.string_of_action action1
            ^ " and "
            ^ Actions.string_of_action action2;
            (Actions.error_action, None)
        | Some span' -> (action1, Some (pas_conf1, pas_conf2, span'))
      end

  let o_trans (pas_conf1, pas_conf2, span) in_move =
    let pas_conf_opt1 = IntLts.o_trans pas_conf1 in_move in
    let pas_conf_opt2 = IntLts.o_trans pas_conf2 in_move in
    match (pas_conf_opt1, pas_conf_opt2) with
    | (None, _) | (_, None) -> None
    | (Some act_conf1, Some act_conf2) -> Some (act_conf1, act_conf2, span)

  let o_trans_gen (pas_conf1, pas_conf2, span) =
    let* (in_move1, act_conf1) = IntLts.o_trans_gen pas_conf1 in
    let* (in_move2, act_conf2) = IntLts.o_trans_gen pas_conf2 in
    match Actions.Moves.unify_move span in_move1 in_move2 with
    | None -> fail ()
    | Some span' -> return (in_move1, (act_conf1, act_conf2, span'))

  let init_aconf comp1 namectxP1 comp2 namectxP2 =
    let init_aconf1 = IntLts.init_aconf comp1 namectxP1 in
    let init_aconf2 = IntLts.init_aconf comp2 namectxP2 in
    let name_l1 = IntLts.Int.IntLang.get_names_from_name_ctx namectxP1 in
    let name_l2 = IntLts.Int.IntLang.get_names_from_name_ctx namectxP2 in
    let span = Util.Namespan.combine (name_l1, name_l2) in
    (init_aconf1, init_aconf2, span)
end
