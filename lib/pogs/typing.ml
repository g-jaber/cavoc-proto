module Make (IntLang : Lang.Interactive.LANG) :
  Lts.Typing.LTS
    with module Moves.Renaming = IntLang.IEnv.Renaming
     and type store_ctx = IntLang.Storectx.t
     and type Moves.copattern =
      IntLang.abstract_normal_form * IntLang.IEnv.Renaming.Namectx.t = struct
  module Moves = Lts.Moves.Make (IntLang : Lang.Interactive.A_NF)
  module BranchMonad = IntLang.BranchMonad

  type store_ctx = IntLang.Storectx.t

  type act_position = {
    storectx: IntLang.Storectx.t;
    namectxO: IntLang.IEnv.Renaming.Namectx.t;
  }

  let act_position_to_yojson ictx =
    `Assoc
      [
        ("storectx", IntLang.Storectx.to_yojson ictx.storectx);
        ("namectxO", IntLang.IEnv.Renaming.Namectx.to_yojson ictx.namectxO);
      ]

  let pp_act_position fmt ictx =
    Format.fprintf fmt "@[⟨Σ: %a |@, ΔO: %a⟩@]" IntLang.Storectx.pp
      ictx.storectx IntLang.IEnv.Renaming.Namectx.pp ictx.namectxO

  type pas_position = {
    storectx: IntLang.Storectx.t;
    namectxP: IntLang.IEnv.Renaming.Namectx.t;
    namectxO: IntLang.IEnv.Renaming.Namectx.t;
  }

  let pas_position_to_yojson ictx =
    `Assoc
      [
        ("storectx", IntLang.Storectx.to_yojson ictx.storectx);
        ("namectxP", IntLang.IEnv.Renaming.Namectx.to_yojson ictx.namectxP);
        ("namectxO", IntLang.IEnv.Renaming.Namectx.to_yojson ictx.namectxO);
      ]

  let pp_pas_position fmt ictx =
    Format.fprintf fmt "@[⟨Σ: %a |@, ΔO: %a |@, ΔP: %a⟩@]" IntLang.Storectx.pp
      ictx.storectx IntLang.IEnv.Renaming.Namectx.pp ictx.namectxO
      IntLang.IEnv.Renaming.Namectx.pp ictx.namectxP

  type position = Active of act_position | Passive of pas_position

  let position_to_yojson = function
    | Active act_position -> act_position_to_yojson act_position
    | Passive pas_position -> pas_position_to_yojson pas_position

  let pp_position fmt = function
    | Active act_position -> pp_act_position fmt act_position
    | Passive pas_position -> pp_pas_position fmt pas_position

  let get_namectxO = function
    | Active pos -> pos.namectxO
    | Passive pos -> pos.namectxO

  (* Active positions do not track the Proponent name context. *)
  let get_namectxP = function
    | Active _ -> IntLang.IEnv.Renaming.Namectx.empty
    | Passive pos -> pos.namectxP

  let get_storectx = function
    | Active pos -> pos.storectx
    | Passive pos -> pos.storectx

  let string_of_position = Format.asprintf "%a" pp_position
  let init_act_pos storectx _ namectxO = Active { storectx; namectxO }

  let init_pas_pos storectx namectxP namectxO =
    Passive { storectx; namectxP; namectxO }

  (* POGS resets the Proponent name context to Δ at each Output move. *)
  let local_context_weakening pos dir lnamectx =
    match dir with
    | Moves.Output -> IntLang.IEnv.Renaming.id lnamectx
    | Moves.Input -> IntLang.IEnv.Renaming.weak_r lnamectx (get_namectxO pos)

  let generate_moves pos =
    Util.Debug.print_debug "Generating moves";
    let open IntLang.BranchMonad in
    match pos with
    | Passive { storectx; namectxP; _ } ->
        let* (a_nf, lnamectx, _) = IntLang.generate_a_nf storectx namectxP in
        let weakening = local_context_weakening pos Moves.Input lnamectx in
        let namectxO = IntLang.IEnv.Renaming.im weakening in
        return
          ( (Moves.Input, (a_nf, lnamectx)),
            weakening,
            Active { storectx; namectxO } )
    | Active { storectx; namectxO } ->
        let* (a_nf, namectxP, namectxO) =
          IntLang.generate_a_nf storectx namectxO in
        let weakening = local_context_weakening pos Moves.Output namectxP in
        return
          ( (Moves.Output, (a_nf, namectxP)),
            weakening,
            Passive { storectx; namectxP; namectxO } )

  let check_move pos ((dir, (a_nf, lnamectx)) : Moves.pol_move) =
    let weakening = local_context_weakening pos dir lnamectx in
    match (dir, pos) with
    | (Moves.Output, Active { storectx; namectxO }) -> begin
        match
          IntLang.type_check_a_nf storectx namectxO
            IntLang.IEnv.Renaming.Namectx.empty (a_nf, lnamectx)
        with
        | Some namectxO ->
            let namectxP = IntLang.IEnv.Renaming.im weakening in
            Some (weakening, Passive { storectx; namectxP; namectxO })
        | None -> None
      end
    | (Moves.Input, Passive { storectx; namectxP; namectxO }) -> begin
        match
          IntLang.type_check_a_nf storectx namectxP namectxO (a_nf, lnamectx)
        with
        | Some _ ->
            let namectxO = IntLang.IEnv.Renaming.im weakening in
            Some (weakening, Active { storectx; namectxO })
        | None -> None
      end
    | _ -> None

  let trigger_move pos ((dir, (_a_nf, lnamectx)) : Moves.pol_move) =
    let weakening = local_context_weakening pos dir lnamectx in
    match (dir, pos) with
    | (Moves.Output, Active { storectx; namectxO }) ->
        ( weakening,
          Passive
            { namectxP= IntLang.IEnv.Renaming.im weakening; storectx; namectxO }
        )
    | (Moves.Input, Passive { storectx; _ }) ->
        ( weakening,
          Active { storectx; namectxO= IntLang.IEnv.Renaming.im weakening } )
    | _ ->
        failwith
          "Trying to trigger a move of the wrong polarity. Please report."
end
