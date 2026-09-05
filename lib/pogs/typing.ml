module Make (IntLang : Lang.Interactive.LANG) : sig
  include
    Lts.Typing.LTS
      with module Moves.Renaming = IntLang.IEnv.Renaming
       and type store_ctx = IntLang.Storectx.t
       and type Moves.copattern =
        IntLang.abstract_normal_form * IntLang.IEnv.Renaming.Namectx.t

  (* The thinning Γ_P↾ ↪ Γ_P keeping the type and polymorphic names, along
     which an Input move resets the Player context. *)
  val reset_thinning : Moves.Renaming.Namectx.t -> Moves.Renaming.t
end = struct
  (* POGS relates heaps a posteriori, so moves are compared without them. *)
  module Moves =
    Lts.Moves.Make
      (IntLang : Lang.Interactive.A_NF)
      (struct
        let compare_heaps = false
      end)
  module BranchMonad = IntLang.BranchMonad
  module Renaming = IntLang.IEnv.Renaming
  module Namectx = Renaming.Namectx

  type store_ctx = IntLang.Storectx.t

  type position = {
    storectx: IntLang.Storectx.t;
    namectxP: Namectx.t;
    namectxO: Namectx.t;
  }

  let position_to_yojson pos =
    `Assoc
      [
        ("storectx", IntLang.Storectx.to_yojson pos.storectx);
        ("namectxP", Namectx.to_yojson pos.namectxP);
        ("namectxO", Namectx.to_yojson pos.namectxO);
      ]

  let pp_position fmt pos =
    Format.fprintf fmt "@[⟨Σ: %a |@, ΔP: %a |@, ΔO: %a⟩@]" IntLang.Storectx.pp
      pos.storectx Namectx.pp pos.namectxP Namectx.pp pos.namectxO

  let string_of_position = Format.asprintf "%a" pp_position
  let get_namectxO pos = pos.namectxO
  let get_namectxP pos = pos.namectxP
  let get_storectx pos = pos.storectx
  let init_act_pos storectx namectxP namectxO = { storectx; namectxP; namectxO }
  let init_pas_pos = init_act_pos

  let reset_thinning namectxP =
    let kept =
      List.filter
        (fun nn ->
          not (Namectx.Names.is_callable nn || Namectx.Names.is_cname nn))
        (Namectx.get_names namectxP) in
    Renaming.of_support namectxP kept

  (* The Player context of the active position reached by an Input move. *)
  let reset namectxP = Renaming.dom (reset_thinning namectxP)

  let local_context_weakening pos dir lnamectx =
    match dir with
    | Moves.Output -> Renaming.weak_r lnamectx pos.namectxP
    | Moves.Input -> Renaming.weak_r lnamectx pos.namectxO

  let generate_moves pos dir =
    let open IntLang.BranchMonad in
    match dir with
    | Moves.Input ->
        let* (a_nf, lnamectx, namectxP) =
          IntLang.generate_a_nf pos.storectx pos.namectxP in
        let weakening = local_context_weakening pos Moves.Input lnamectx in
        let namectxO = Renaming.im weakening in
        return
          ( (Moves.Input, (a_nf, lnamectx)),
            weakening,
            { pos with namectxO; namectxP= reset namectxP } )
    | Moves.Output ->
        let* (a_nf, lnamectx, namectxO) =
          IntLang.generate_a_nf pos.storectx pos.namectxO in
        let weakening = local_context_weakening pos Moves.Output lnamectx in
        let namectxP = Renaming.im weakening in
        return
          ( (Moves.Output, (a_nf, lnamectx)),
            weakening,
            { pos with namectxO; namectxP } )

  let check_move pos ((dir, (a_nf, lnamectx)) : Moves.pol_move) =
    let weakening = local_context_weakening pos dir lnamectx in
    match dir with
    | Moves.Output -> begin
        match
          IntLang.type_check_a_nf pos.storectx pos.namectxO pos.namectxP
            (a_nf, lnamectx)
        with
        | Some namectxO ->
            let namectxP = Renaming.im weakening in
            Some (weakening, { pos with namectxP; namectxO })
        | None -> None
      end
    | Moves.Input -> begin
        match
          IntLang.type_check_a_nf pos.storectx pos.namectxP pos.namectxO
            (a_nf, lnamectx)
        with
        | Some namectxP ->
            let namectxO = Renaming.im weakening in
            Some (weakening, { pos with namectxP= reset namectxP; namectxO })
        | None -> None
      end

  let trigger_move pos ((dir, (_a_nf, lnamectx)) : Moves.pol_move) =
    let weakening = local_context_weakening pos dir lnamectx in
    match dir with
    | Moves.Output ->
        (weakening, { pos with namectxP= Renaming.im weakening })
    | Moves.Input ->
        ( weakening,
          {
            pos with
            namectxO= Renaming.im weakening;
            namectxP= reset pos.namectxP;
          } )
end
