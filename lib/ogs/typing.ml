module Make (IntLang : Lang.Interactive.LANG) :
  Lts.Typing.LTS
    with module Moves.Renaming = IntLang.IEnv.Renaming
     and type store_ctx = IntLang.Storectx.t
     and type Moves.move =
      IntLang.abstract_normal_form * IntLang.IEnv.Renaming.t = struct
  module Moves = Lts.Moves.Make ((IntLang : Lts.Moves.A_NF))
  module BranchMonad = IntLang.BranchMonad

  type store_ctx = IntLang.Storectx.t
  type status = Active | Passive

  type position = {
    status: status;
    storectx: IntLang.Storectx.t;
    namectxP: IntLang.IEnv.Renaming.Namectx.t;
    namectxO: IntLang.IEnv.Renaming.Namectx.t;
  }

  let get_namectxO pos = pos.namectxO
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

  let init_act_pos storectx namectxP namectxO =
    { status= Active; storectx; namectxP; namectxO }

  let init_pas_pos storectx namectxP namectxO =
    { status= Passive; storectx; namectxP; namectxO }

  let generate_moves pos =
    Util.Debug.print_debug "Generating moves";
    let open IntLang.BranchMonad in
    match pos with
    | { status= Passive; storectx; namectxP; namectxO } ->
        let* (a_nf, lnamectx, namectxP) =
          IntLang.generate_a_nf storectx namectxP in
        (* We get renaming : Δ → Γₒ + Δ with Δ=lnamectx and Γₒ=namectxO *)
        let renaming = IntLang.IEnv.Renaming.weak_r lnamectx namectxO in
        (* now namectxO = Γₒ + Δ *)
        let namectxO = IntLang.IEnv.Renaming.im renaming in
        Util.Debug.print_debug @@ "The new move " ^ (IntLang.string_of_a_nf "?" a_nf) ^ " is producing the new name context :"
        ^ IntLang.IEnv.Renaming.Namectx.to_string lnamectx
        ^ " giving the updated Opponent name context "
        ^ IntLang.IEnv.Renaming.Namectx.to_string namectxO;
        return
          ( (Moves.Input, (a_nf, renaming)),
            { status= Active; storectx; namectxO; namectxP } )
    | { status= Active; storectx; namectxP; namectxO } ->
        let* (a_nf, lnamectx, namectxO) =
          IntLang.generate_a_nf storectx namectxO in
        let renaming = IntLang.IEnv.Renaming.weak_r lnamectx namectxP in
        let namectxP = IntLang.IEnv.Renaming.im renaming in
        Util.Debug.print_debug @@ "New Proponent name context :"
        ^ IntLang.IEnv.Renaming.Namectx.to_string lnamectx
        ^ " and "
        ^ IntLang.IEnv.Renaming.Namectx.to_string namectxP;
        return
          ( (Moves.Input, (a_nf, renaming)),
            { status= Passive; storectx; namectxO; namectxP } )

  let check_move pos (dir, (a_nf, renaming)) =
    let lnamectx = Moves.Renaming.dom renaming in
    match (dir, pos) with
    | (Moves.Output, { status= Active; storectx; namectxO; _ }) -> begin
        match IntLang.type_check_a_nf storectx namectxO (a_nf, lnamectx) with
        | Some namectxO ->
            let namectxP = Moves.Renaming.im renaming in
            Some { status= Passive; storectx; namectxP; namectxO }
        | None -> None
      end
    | (Moves.Input, { status= Passive; storectx; namectxP; _ }) -> begin
        match IntLang.type_check_a_nf storectx namectxP (a_nf, lnamectx) with
        | Some namectxP ->
            let namectxO = Moves.Renaming.im renaming in
            Some { status= Active; storectx; namectxP; namectxO }
        | None -> None
      end
    | _ -> None

  let trigger_move pos (dir, move) =
    let lnamectx = Moves.get_namectx move in
    match (dir, pos) with
    | (Moves.Output, { status= Active; storectx; namectxP; namectxO }) ->
        let namectxP = IntLang.IEnv.Renaming.Namectx.concat lnamectx namectxP in
          Util.Debug.print_debug @@ "After trigger, new Proponent name context :"
        ^ IntLang.IEnv.Renaming.Namectx.to_string namectxP
        ^ " and previous Opponent name context"
        ^ IntLang.IEnv.Renaming.Namectx.to_string namectxO;
        { status= Passive; storectx; namectxP; namectxO }
    | (Moves.Input, { status= Passive; storectx; namectxP; namectxO }) ->
        let namectxO = IntLang.IEnv.Renaming.Namectx.concat lnamectx namectxO in
          Util.Debug.print_debug @@ "After trigger, new Opponent name context :"
        ^ IntLang.IEnv.Renaming.Namectx.to_string namectxO
        ^ " and previous Proponent name context"
        ^ IntLang.IEnv.Renaming.Namectx.to_string namectxP;
        { status= Active; storectx; namectxP; namectxO }
    | _ ->
        failwith
          "Trying to trigger a move of the wrong polarity. Please report."
end
