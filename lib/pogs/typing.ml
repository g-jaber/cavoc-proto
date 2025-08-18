module Make (IntLang : Lang.Interactive.LANG) :
  Lts.Typing.LTS
    with type Moves.Names.name = IntLang.Names.name
     and type name_ctx = IntLang.name_ctx
     and type store_ctx = IntLang.store_ctx
     and type Moves.move = IntLang.abstract_normal_form = struct
  module Moves = Lts.Moves.Make (IntLang)
  module BranchMonad = IntLang.BranchMonad

  type name_ctx = IntLang.name_ctx
  type store_ctx = IntLang.store_ctx

  let domain_of_name_ctx namectx = IntLang.get_names_from_name_ctx namectx

  type act_position = {
    storectx: IntLang.store_ctx;
    namectxO: IntLang.name_ctx;
  }

  let act_position_to_yojson ictx =
    `Assoc
      [
        ("storectx", `String (IntLang.string_of_store_ctx ictx.storectx));
        ("namectxO", IntLang.name_ctx_to_yojson ictx.namectxO);
      ]

  let pp_act_position fmt ictx =
    Format.fprintf fmt "@[⟨Σ: %a |@, ΔO: %a⟩@]" IntLang.pp_store_ctx
      ictx.storectx IntLang.pp_name_ctx ictx.namectxO

  type pas_position = {
    storectx: IntLang.store_ctx;
    namectxP: IntLang.name_ctx;
    namectxO: IntLang.name_ctx;
  }

  let pas_position_to_yojson ictx =
    `Assoc
      [
        ("storectx", `String (IntLang.string_of_store_ctx ictx.storectx));
        ("namectxP", IntLang.name_ctx_to_yojson ictx.namectxP);
        ("namectxO", IntLang.name_ctx_to_yojson ictx.namectxO);
      ]

  let pp_pas_position fmt ictx =
    Format.fprintf fmt "@[⟨Σ: %a |@, ΔO: %a |@, ΔP: %a⟩@]" IntLang.pp_store_ctx
      ictx.storectx IntLang.pp_name_ctx ictx.namectxO IntLang.pp_name_ctx
      ictx.namectxP

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

  let get_storectx = function
    | Active pos -> pos.storectx
    | Passive pos -> pos.storectx

  let string_of_position = Format.asprintf "%a" pp_position
  let init_act_pos storectx _ namectxO = Active { storectx; namectxO }

  let init_pas_pos storectx namectxP namectxO =
    Passive { storectx; namectxP; namectxO }

  let generate_moves pos =
    Util.Debug.print_debug "Generating moves";
    let open IntLang.BranchMonad in
    match pos with
    | Passive { storectx; namectxP; namectxO } ->
        let* (a_nf, lnamectx, _) = IntLang.generate_a_nf storectx namectxP in
        let namectxO = IntLang.concat_name_ctx lnamectx namectxO in
        return (((Moves.Input, a_nf), lnamectx), Active { storectx; namectxO })
    | Active { storectx; namectxO } ->
        let* (a_nf, namectxP, namectxO) =
          IntLang.generate_a_nf storectx namectxO in
        return
          ( ((Moves.Output, a_nf), namectxP),
            Passive { storectx; namectxP; namectxO } )

  let check_move pos ((dir, a_nf), _) =
    match (dir, pos) with
    | (Moves.Output, Active { storectx; namectxO }) -> begin
        match IntLang.type_check_a_nf IntLang.empty_name_ctx namectxO a_nf with
        | Some (namectxP, namectxO) ->
            Some (Passive { storectx; namectxP; namectxO })
        | None -> None
      end
    | (Moves.Input, Passive { storectx; namectxO; namectxP }) -> begin
        match IntLang.type_check_a_nf namectxP namectxO a_nf with
        | Some (namectxO, _) -> Some (Active { storectx; namectxO })
        | None -> None
      end
    | _ -> None

  let trigger_move pos ((dir, _), lnamectx) =
    match (dir, pos) with
    | (Moves.Output, Active { storectx; namectxO }) ->
        Passive { namectxP= lnamectx; storectx; namectxO }
    | (Moves.Input, Passive { storectx; namectxO; _ }) ->
        let namectxO = IntLang.concat_name_ctx lnamectx namectxO in
        Active { storectx; namectxO }
    | _ ->
        failwith
          "Trying to trigger a move of the wrong polarity. Please report."
end
