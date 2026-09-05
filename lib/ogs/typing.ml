module Make (IntLang : Lang.Interactive.LANG) :
  Lts.Typing.LTS
    with module Moves.Renaming = IntLang.IEnv.Renaming
     and module BranchMonad = IntLang.BranchMonad
     and type store_ctx = IntLang.Storectx.t
     and type Moves.copattern =
      IntLang.abstract_normal_form * IntLang.IEnv.Renaming.Namectx.t = struct
  module Moves =
    Lts.Moves.Make
      (IntLang : Lang.Interactive.A_NF)
      (struct
        let compare_heaps = true
      end)
  module BranchMonad = IntLang.BranchMonad

  type store_ctx = IntLang.Storectx.t

  type position = {
    storectx: IntLang.Storectx.t;
    namectxP: IntLang.IEnv.Renaming.Namectx.t;
    namectxO: IntLang.IEnv.Renaming.Namectx.t;
  }

  let get_namectxO pos = pos.namectxO
  let get_namectxP pos = pos.namectxP
  let get_storectx pos = pos.storectx

  let position_to_yojson pos =
    `Assoc
      [
        ("storectx", IntLang.Storectx.to_yojson pos.storectx);
        ("namectxP", IntLang.IEnv.Renaming.Namectx.to_yojson pos.namectxP);
        ("namectxO", IntLang.IEnv.Renaming.Namectx.to_yojson pos.namectxO);
      ]

  let pp_position fmt pos =
    Format.fprintf fmt "@[⟨Σ: %a |@, ΔO: %a |@, ΔP: %a⟩@]" IntLang.Storectx.pp
      pos.storectx IntLang.IEnv.Renaming.Namectx.pp pos.namectxO
      IntLang.IEnv.Renaming.Namectx.pp pos.namectxP

  let string_of_position = Format.asprintf "%a" pp_position
  let init_act_pos storectx namectxP namectxO = { storectx; namectxP; namectxO }
  let init_pas_pos = init_act_pos

  let local_context_weakening pos dir lnamectx =
    match dir with
    | Moves.Output -> IntLang.IEnv.Renaming.weak_r lnamectx pos.namectxP
    | Moves.Input -> IntLang.IEnv.Renaming.weak_r lnamectx pos.namectxO

  let generate_moves pos dir =
    Util.Debug.print_debug "Generating moves";
    let open IntLang.BranchMonad in
    match dir with
    | Moves.Input ->
        let* (a_nf, lnamectx, namectxP) =
          IntLang.generate_a_nf pos.storectx pos.namectxP in
        (* We get weakening : Δ → Γₒ + Δ with Δ=lnamectx and Γₒ=namectxO *)
        let weakening = local_context_weakening pos Moves.Input lnamectx in
        (* now namectxO = Γₒ + Δ *)
        let namectxO = IntLang.IEnv.Renaming.im weakening in
        Util.Debug.print_debug @@ "The new move "
        ^ IntLang.string_of_a_nf "?" a_nf
        ^ " is producing the new name context :"
        ^ IntLang.IEnv.Renaming.Namectx.to_string lnamectx
        ^ " giving the updated Opponent name context "
        ^ IntLang.IEnv.Renaming.Namectx.to_string namectxO;
        return
          ( (Moves.Input, (a_nf, lnamectx)),
            weakening,
            { pos with namectxO; namectxP } )
    | Moves.Output ->
        let* (a_nf, lnamectx, namectxO) =
          IntLang.generate_a_nf pos.storectx pos.namectxO in
        let weakening = local_context_weakening pos Moves.Output lnamectx in
        let namectxP = IntLang.IEnv.Renaming.im weakening in
        Util.Debug.print_debug @@ "New Proponent name context :"
        ^ IntLang.IEnv.Renaming.Namectx.to_string lnamectx
        ^ " and "
        ^ IntLang.IEnv.Renaming.Namectx.to_string namectxP;
        return
          ( (Moves.Output, (a_nf, lnamectx)),
            weakening,
            { pos with namectxO; namectxP } )

  let check_move pos ((dir, (a_nf, lnamectx)) : Moves.pol_move) =
    let weakening = local_context_weakening pos dir lnamectx in
    match dir with
    (* A Proponent move is typed with the two contexts swapped, like its
       generation. *)
    | Moves.Output -> begin
        match
          IntLang.type_check_a_nf pos.storectx pos.namectxO pos.namectxP
            (a_nf, lnamectx)
        with
        | Some namectxO ->
            let namectxP = IntLang.IEnv.Renaming.im weakening in
            Some (weakening, { pos with namectxP; namectxO })
        | None -> None
      end
    | Moves.Input -> begin
        match
          IntLang.type_check_a_nf pos.storectx pos.namectxP pos.namectxO
            (a_nf, lnamectx)
        with
        | Some namectxP ->
            let namectxO = IntLang.IEnv.Renaming.im weakening in
            Some (weakening, { pos with namectxP; namectxO })
        | None -> None
      end

  (* Beware that trigger_move does not update correctly the positions when
    some resources are consumed by the move *)
  let trigger_move pos ((dir, (_a_nf, lnamectx)) : Moves.pol_move) =
    let weakening = local_context_weakening pos dir lnamectx in
    match dir with
    | Moves.Output ->
        let namectxP = IntLang.IEnv.Renaming.im weakening in
        Util.Debug.print_debug @@ "After trigger, new Proponent name context :"
        ^ IntLang.IEnv.Renaming.Namectx.to_string namectxP
        ^ " and Opponent name context stays "
        ^ IntLang.IEnv.Renaming.Namectx.to_string pos.namectxO;
        (weakening, { pos with namectxP })
    | Moves.Input ->
        let namectxO = IntLang.IEnv.Renaming.im weakening in
        Util.Debug.print_debug @@ "After trigger, new Opponent name context :"
        ^ IntLang.IEnv.Renaming.Namectx.to_string namectxO
        ^ " and Proponent name context stays "
        ^ IntLang.IEnv.Renaming.Namectx.to_string pos.namectxP;
        (weakening, { pos with namectxO })
end
