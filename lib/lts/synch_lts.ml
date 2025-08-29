module Make (IntLts : Bipartite.INT_LTS) :
  Bipartite.INT_LTS
    with type Moves.Namectx.t = IntLts.Moves.Namectx.t
     and type opconf = IntLts.opconf * IntLts.opconf
     and type store = IntLts.store * IntLts.store
     and type interactive_env = IntLts.interactive_env * IntLts.interactive_env =
struct
  module OBranchingMonad = IntLts.OBranchingMonad
  module EvalMonad = IntLts.EvalMonad

  type opconf = IntLts.opconf * IntLts.opconf
  type store = IntLts.store * IntLts.store
  type interactive_env = IntLts.interactive_env * IntLts.interactive_env

  module Moves = struct
    module Namectx = IntLts.Moves.Namectx

    type move = IntLts.Moves.move * IntLts.Moves.move
    type direction = Input | Output

    let convert_dir = function
      | Input -> IntLts.Moves.Input
      | Output -> IntLts.Moves.Output

    let switch = function Input -> Output | Output -> Input

    type pol_move = direction * move

    let pp_move fmt (move, _) = IntLts.Moves.pp_move fmt move

    let pp_pol_move fmt (dir, (move, _)) =
      IntLts.Moves.pp_pol_move fmt (convert_dir dir, move)

    let string_of_move (move, _) = IntLts.Moves.string_of_move move

    let string_of_pol_move (dir, (move, _)) =
      IntLts.Moves.string_of_pol_move (convert_dir dir, move)

    let switch_direction (p, d) = (switch p, d)
    let get_subject_name (move, _) = IntLts.Moves.get_subject_name move
    let get_namectx (move, _) = IntLts.Moves.get_namectx move

    let unify_move span (move1, _) (move2, _) =
      IntLts.Moves.unify_move span move1 move2

    let unify_pol_move span (dir1, move1) (dir2, move2) =
      if dir1 = dir2 then unify_move span move1 move2 else None
  end

  type active_conf =
    IntLts.active_conf
    * IntLts.active_conf
    * Moves.Namectx.Names.name Util.Namespan.namespan

  type passive_conf =
    IntLts.passive_conf
    * IntLts.passive_conf
    * Moves.Namectx.Names.name Util.Namespan.namespan

  let passive_conf_to_yojson _ = failwith "Not implemented"

  type conf = Active of active_conf | Passive of passive_conf

  let pp_active_conf fmt (act_conf1, act_conf2, namespan) =
    Format.fprintf fmt "@[⟨%a |@, %a |@, %a⟩]" IntLts.pp_active_conf act_conf1
      IntLts.pp_active_conf act_conf2
      (Util.Namespan.pp_namespan Moves.Namectx.Names.pp_name)
      namespan

  let pp_passive_conf fmt (pas_conf1, pas_conf2, namespan) =
    Format.fprintf fmt "@[⟨%a |@, %a |@, %a⟩]" IntLts.pp_passive_conf pas_conf1
      IntLts.pp_passive_conf pas_conf2
      (Util.Namespan.pp_namespan Moves.Namectx.Names.pp_name)
      namespan

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let equiv_act_conf (act_conf1a, act_conf2a, _) (act_conf1b, act_conf2b, _) =
    IntLts.equiv_act_conf act_conf1a act_conf1b
    && IntLts.equiv_act_conf act_conf2a act_conf2b

  let fold_moves (dir1, move1) (dir2, move2) =
    match (dir1, dir2) with
    | (IntLts.Moves.Input, IntLts.Moves.Input) -> (Moves.Input, (move1, move2))
    | (IntLts.Moves.Output, IntLts.Moves.Output) ->
        (Moves.Output, (move1, move2))
    | _ ->
        failwith
          "Error: trying to fold moves with different directions. Please \
           report."

  let unfold_move (dir, (move1, move2)) =
    ((Moves.convert_dir dir, move1), (Moves.convert_dir dir, move2))

  let p_trans (act_conf1, act_conf2, span) =
    let open EvalMonad in
    let* (move1, pas_conf1) = IntLts.p_trans act_conf1 in
    let* (move2, pas_conf2) = IntLts.p_trans act_conf2 in
    match IntLts.Moves.unify_pol_move span move1 move2 with
    | None ->
        Util.Debug.print_debug @@ "Cannot synchronize output moves "
        ^ IntLts.Moves.string_of_pol_move move1
        ^ " and "
        ^ IntLts.Moves.string_of_pol_move move2;
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
    let* (move1, act_conf1) = IntLts.o_trans_gen pas_conf1 in
    let* (move2, act_conf2) = IntLts.o_trans_gen pas_conf2 in
    match IntLts.Moves.unify_pol_move span move1 move2 with
    | None -> fail ()
    | Some span' ->
        let move = fold_moves move1 move2 in
        return (move, (act_conf1, act_conf2, span'))

  let init_aconf (opconf1, opconf2) namectxP =
    let init_aconf1 = IntLts.init_aconf opconf1 namectxP in
    let init_aconf2 = IntLts.init_aconf opconf2 namectxP in
    let name_l = IntLts.Moves.Namectx.get_names namectxP in
    let span = Util.Namespan.combine (name_l, name_l) in
    (* Not needed*)
    (init_aconf1, init_aconf2, span)

  let init_pconf (store1, store2) (ienv1, ienv2) namectxP namectxO =
    let init_pconf1 = IntLts.init_pconf store1 ienv1 namectxP namectxO in
    let init_pconf2 = IntLts.init_pconf store2 ienv2 namectxP namectxO in
    let name_l = IntLts.Moves.Namectx.get_names namectxP in
    let span = Util.Namespan.combine (name_l, name_l) in
    (* Should we also use namectxO1 and namectxO2 to build a span ? Or should they be checked to be equal ?*)
    (init_pconf1, init_pconf2, span)
end
