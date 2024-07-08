module type INT_LTS = sig
  module Int : Interactive.INT
  include Bipartite.LTS (*with module Actions = Int.Actions*)

  (* init_aconf creates an configuration from two computations and a two name context for Opponent.
     Its store, interactive env, and name context for Proponent are all set to empty*)
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

  module A_nf = struct
    module Name = IntLts.Int.IntLang.Name

    type abstract_normal_form =
      IntLts.Int.IntLang.abstract_normal_form
      * IntLts.Int.IntLang.abstract_normal_form
    (*IntLts.Actions.Moves.kdata
      * IntLts.Actions.Moves.kdata*)

    let string_of_a_nf sep (kdata1, _) =
      IntLts.Int.IntLang.string_of_a_nf sep kdata1
    (*^ ","
      ^ IntLts.Int.IntLang.string_of_a_nf sep kdata2*)

    let get_subject_name (kdata1, _) =
      IntLts.Int.IntLang.get_subject_name kdata1

    let get_support (kdata1, _) = IntLts.Int.IntLang.get_support kdata1
    let is_equiv_a_nf _ _ = failwith "eq"
  end

  (* *)
  module Int = IntLts.Int
  module Actions = Actions.Make (Moves.Make (A_nf))

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

  let pp_passive_conf _ (_, _, _) = failwith "Not yet implemented"

  let equiv_act_conf (act_conf1a, act_conf2a, _) (act_conf1b, act_conf2b, _) =
    IntLts.equiv_act_conf act_conf1a act_conf1b
    && IntLts.equiv_act_conf act_conf2a act_conf2b

  let convert_dir = function
    | Actions.Moves.Input -> Int.Actions.Moves.Input
    | Actions.Moves.Output -> Int.Actions.Moves.Output
    | Actions.Moves.None -> Int.Actions.Moves.None

  let fold_moves move1 move2 =
    let kdata1 = Int.Actions.Moves.get_kdata move1 in
    let kdata2 = Int.Actions.Moves.get_kdata move2 in
    let dir1 = Int.Actions.Moves.get_direction move1 in
    let dir2 = Int.Actions.Moves.get_direction move2 in
    match (dir1, dir2) with
    | (Int.Actions.Moves.Input, Int.Actions.Moves.Input) ->
        Actions.Moves.build (Actions.Moves.Input, (kdata1, kdata2))
    | (Int.Actions.Moves.Output, Int.Actions.Moves.Output) ->
        Actions.Moves.build (Actions.Moves.Output, (kdata1, kdata2))
    | _ ->
        failwith
          "Error: trying to fold moves with different directions. Please \
           report."

  let unfold_move move =
    let (kdata1, kdata2) = Actions.Moves.get_kdata move in
    let dir = convert_dir @@ Actions.Moves.get_direction move in
    let move1 = Int.Actions.Moves.build (dir, kdata1) in
    let move2 = Int.Actions.Moves.build (dir, kdata2) in
    (move1, move2)

  let p_trans (act_conf1, act_conf2, span) =
    let (action1, pas_conf_opt1) = IntLts.p_trans act_conf1 in
    let (action2, pas_conf_opt2) = IntLts.p_trans act_conf2 in
    match (pas_conf_opt1, pas_conf_opt2) with
    | (None, _) | (_, None) -> (Actions.error_action, None)
    | (Some pas_conf1, Some pas_conf2) -> begin
        match (action1, action2) with
        | (Vis move1, Vis move2) -> begin
            match Int.Actions.Moves.unify_move span move1 move2 with
            | None ->
                Util.Debug.print_debug @@ "Cannot synchronize output actions "
                ^ IntLts.Int.Actions.string_of_action action1
                ^ " and "
                ^ IntLts.Int.Actions.string_of_action action2;
                (Actions.error_action, None)
            | Some span' ->
                let move = fold_moves move1 move2 in
                (Vis move, Some (pas_conf1, pas_conf2, span'))
          end
        | (PDiv, PDiv) -> (PDiv, None)
        | (PError, PError) -> (PError, None)
        | _ -> (Actions.error_action, None)
        (* To be reworked *)
      end

  let o_trans (pas_conf1, pas_conf2, span) in_move =
    let (in_move1, in_move2) = unfold_move in_move in
    let pas_conf_opt1 = IntLts.o_trans pas_conf1 in_move1 in
    let pas_conf_opt2 = IntLts.o_trans pas_conf2 in_move2 in
    match (pas_conf_opt1, pas_conf_opt2) with
    | (None, _) | (_, None) -> None
    | (Some act_conf1, Some act_conf2) -> Some (act_conf1, act_conf2, span)

  let o_trans_gen (pas_conf1, pas_conf2, span) =
    let* (in_move1, act_conf1) = IntLts.o_trans_gen pas_conf1 in
    let* (in_move2, act_conf2) = IntLts.o_trans_gen pas_conf2 in
    match Int.Actions.Moves.unify_move span in_move1 in_move2 with
    | None -> fail ()
    | Some span' ->
        let move = fold_moves in_move1 in_move2 in
        return (move, (act_conf1, act_conf2, span'))

  let init_aconf comp1 namectxP1 comp2 namectxP2 =
    let init_aconf1 = IntLts.init_aconf comp1 namectxP1 in
    let init_aconf2 = IntLts.init_aconf comp2 namectxP2 in
    let name_l1 = IntLts.Int.IntLang.get_names_from_name_ctx namectxP1 in
    let name_l2 = IntLts.Int.IntLang.get_names_from_name_ctx namectxP2 in
    let span = Util.Namespan.combine (name_l1, name_l2) in
    (init_aconf1, init_aconf2, span)
end
