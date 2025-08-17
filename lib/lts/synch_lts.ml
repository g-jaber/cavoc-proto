module type INT_LTS = sig
  module Int : Interactive.INT
  include Bipartite.LTS

  (* init_aconf creates an configuration from two computations and a two name context for Opponent.
     Its store, interactive env, and name context for Proponent are all set to empty*)
  val init_aconf :
    Int.IntLang.opconf ->
    Int.IntLang.name_ctx ->
    Int.IntLang.opconf ->
    Int.IntLang.name_ctx ->
    active_conf
end

module Make (IntLts : Bipartite.INT_LTS) :
  INT_LTS with module Int = IntLts.Int = struct
  module OBranchingMonad = IntLts.OBranchingMonad
  module EvalMonad = IntLts.EvalMonad

  module A_nf = struct
    module Name = IntLts.Int.IntLang.Name

    type abstract_normal_form =
      IntLts.Int.IntLang.abstract_normal_form
      * IntLts.Int.IntLang.abstract_normal_form

    let pp_a_nf ~pp_dir fmt (kdata1, _) =
      IntLts.Int.IntLang.pp_a_nf ~pp_dir fmt kdata1

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
  module Moves = Moves.Make (A_nf)

  type active_conf =
    IntLts.active_conf
    * IntLts.active_conf
    * IntLts.Int.Name.name Util.Namespan.namespan

  type passive_conf =
    IntLts.passive_conf
    * IntLts.passive_conf
    * IntLts.Int.Name.name Util.Namespan.namespan

  let passive_conf_to_yojson _ = failwith "Not implemented"

  type conf = Active of active_conf | Passive of passive_conf

  let pp_active_conf fmt (act_conf1, act_conf2, namespan) =
    Format.fprintf fmt "@[⟨%a |@, %a |@, %a⟩]" IntLts.pp_active_conf act_conf1
      IntLts.pp_active_conf act_conf2
      (Util.Namespan.pp_namespan IntLts.Int.Name.pp_name)
      namespan

  let pp_passive_conf fmt (pas_conf1, pas_conf2, namespan) =
    Format.fprintf fmt "@[⟨%a |@, %a |@, %a⟩]" IntLts.pp_passive_conf pas_conf1
      IntLts.pp_passive_conf pas_conf2
      (Util.Namespan.pp_namespan IntLts.Int.Name.pp_name)
      namespan

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let equiv_act_conf (act_conf1a, act_conf2a, _) (act_conf1b, act_conf2b, _) =
    IntLts.equiv_act_conf act_conf1a act_conf1b
    && IntLts.equiv_act_conf act_conf2a act_conf2b

  let convert_dir = function
    | Moves.Input -> IntLts.Moves.Input
    | Moves.Output -> IntLts.Moves.Output
    | Moves.None -> IntLts.Moves.None

  let fold_moves move1 move2 =
    let kdata1 = IntLts.Moves.get_kdata move1 in
    let kdata2 = IntLts.Moves.get_kdata move2 in
    let dir1 = IntLts.Moves.get_direction move1 in
    let dir2 = IntLts.Moves.get_direction move2 in
    match (dir1, dir2) with
    | (IntLts.Moves.Input, IntLts.Moves.Input) ->
        Moves.build (Moves.Input, (kdata1, kdata2))
    | (IntLts.Moves.Output, IntLts.Moves.Output) ->
        Moves.build (Moves.Output, (kdata1, kdata2))
    | _ ->
        failwith
          "Error: trying to fold moves with different directions. Please \
           report."

  let unfold_move move =
    let (kdata1, kdata2) = Moves.get_kdata move in
    let dir = convert_dir @@ Moves.get_direction move in
    let move1 = IntLts.Moves.build (dir, kdata1) in
    let move2 = IntLts.Moves.build (dir, kdata2) in
    (move1, move2)

  let p_trans (act_conf1, act_conf2, span) =
    let open EvalMonad in
    let* (move1, pas_conf1) = IntLts.p_trans act_conf1 in
    let* (move2, pas_conf2) = IntLts.p_trans act_conf2 in
    match IntLts.Moves.unify_move span move1 move2 with
    | None ->
        Util.Debug.print_debug @@ "Cannot synchronize output moves "
        ^ IntLts.Moves.string_of_move move1
        ^ " and "
        ^ IntLts.Moves.string_of_move move2;
        EvalMonad.fail ()
    | Some span' ->
        let move = fold_moves move1 move2 in
        return (move, (pas_conf1, pas_conf2, span'))

  let o_trans (pas_conf1, pas_conf2, span) in_move =
    let (in_move1, in_move2) = unfold_move in_move in
    let pas_conf_opt1 = IntLts.o_trans pas_conf1 in_move1 in
    let pas_conf_opt2 = IntLts.o_trans pas_conf2 in_move2 in
    match (pas_conf_opt1, pas_conf_opt2) with
    | (None, _) | (_, None) -> None
    | (Some act_conf1, Some act_conf2) -> Some (act_conf1, act_conf2, span)

  let o_trans_gen (pas_conf1, pas_conf2, span) =
    let open OBranchingMonad in
    let* (in_move1, act_conf1) = IntLts.o_trans_gen pas_conf1 in
    let* (in_move2, act_conf2) = IntLts.o_trans_gen pas_conf2 in
    match IntLts.Moves.unify_move span in_move1 in_move2 with
    | None -> fail ()
    | Some span' ->
        let move = fold_moves in_move1 in_move2 in
        return (move, (act_conf1, act_conf2, span'))

  let init_aconf opconf1 namectxP1 opconf2 namectxP2 =
    let init_aconf1 = IntLts.init_aconf opconf1 namectxP1 in
    let init_aconf2 = IntLts.init_aconf opconf2 namectxP2 in
    let name_l1 = IntLts.Int.IntLang.get_names_from_name_ctx namectxP1 in
    let name_l2 = IntLts.Int.IntLang.get_names_from_name_ctx namectxP2 in
    let span = Util.Namespan.combine (name_l1, name_l2) in
    (init_aconf1, init_aconf2, span)
end
