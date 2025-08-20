module Make (IntLang : Lang.Interactive.LANG) :
  Lts.Typing.LTS
    with type Moves.Names.name = IntLang.Names.name
     and type name_ctx = IntLang.Namectx.t
     and type store_ctx = IntLang.store_ctx
     and type Moves.move = IntLang.abstract_normal_form = struct
  module Moves = Lts.Moves.Make (IntLang)
  module BranchMonad = IntLang.BranchMonad

  type name_ctx = IntLang.Namectx.t
  type store_ctx = IntLang.store_ctx

  let domain_of_name_ctx namectx =
    IntLang.Namectx.get_names namectx

  type status = Active | Passive

  type position = {
    status: status;
    storectx: IntLang.store_ctx;
    namectxP: IntLang.Namectx.t;
    namectxO: IntLang.Namectx.t;
  }

  let get_namectxO pos = pos.namectxO
  let get_storectx pos = pos.storectx

  let position_to_yojson pos =
    `Assoc
      [
        ("storectx", `String (IntLang.string_of_store_ctx pos.storectx));
        ("namectxP", IntLang.Namectx.to_yojson pos.namectxP);
        ("namectxO", IntLang.Namectx.to_yojson pos.namectxO);
      ]

  let pp_position fmt pos =
    Format.fprintf fmt "@[⟨Σ: %a |@, ΔO: %a |@, ΔP: %a⟩@]" IntLang.pp_store_ctx
      pos.storectx IntLang.Namectx.pp pos.namectxO
      IntLang.Namectx.pp pos.namectxP

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
        let namectxO = IntLang.Namectx.concat lnamectx namectxO in
        Util.Debug.print_debug @@ "New Opponent name context :"
        ^ IntLang.Namectx.to_string lnamectx
        ^ " and "
        ^ IntLang.Namectx.to_string namectxO;
        return
          ( ((Moves.Input, a_nf), lnamectx),
            { status= Active; storectx; namectxO; namectxP } )
    | { status= Active; storectx; namectxP; namectxO } ->
        let* (a_nf, lnamectx, namectxO) =
          IntLang.generate_a_nf storectx namectxO in
        let namectxO = IntLang.Namectx.concat lnamectx namectxO in
        Util.Debug.print_debug @@ "New Proponent name context :"
        ^ IntLang.Namectx.to_string lnamectx
        ^ " and "
        ^ IntLang.Namectx.to_string namectxO;
        return
          ( ((Moves.Input, a_nf), lnamectx),
            { status= Passive; storectx; namectxO; namectxP } )

  let check_move pos ((dir, a_nf), _) =
    match (dir, pos) with
    | (Moves.Output, { status= Active; storectx; namectxP; namectxO }) -> begin
        match IntLang.type_check_a_nf namectxO namectxP a_nf with
        | Some (namectxP, namectxO) ->
            Some { status= Passive; storectx; namectxP; namectxO }
        | None -> None
      end
    | (Moves.Input, { status= Passive; storectx; namectxP; namectxO }) -> begin
        match IntLang.type_check_a_nf namectxP namectxO a_nf with
        | Some (namectxO, _) ->
            Some { status= Active; storectx; namectxP; namectxO }
        | None -> None
      end
    | _ -> None

  let trigger_move pos ((dir, _), lnamectx) =
    match (dir, pos) with
    | (Moves.Output, { status= Active; storectx; namectxP; namectxO }) ->
        let namectxP = IntLang.Namectx.concat lnamectx namectxP in
        { status= Passive; storectx; namectxP; namectxO }
    | (Moves.Input, { status= Passive; storectx; namectxP; namectxO }) ->
        let namectxO = IntLang.Namectx.concat lnamectx namectxO in
        { status= Active; storectx; namectxP; namectxO }
    | _ ->
        failwith
          "Trying to trigger a move of the wrong polarity. Please report."
end
