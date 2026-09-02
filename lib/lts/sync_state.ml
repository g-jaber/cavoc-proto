(* Synchronization state of the disclosure-free open composition. *)

module Make (Renaming : Lang.Renaming.WEAKENING) = struct
  type side = Left | Right
  type table = Renaming.Namectx.Names.name Util.Namespan.namespan

  type t = {
    (* The two sides of Shared_LR, by introducing side: a name the left
       component introduces sits in Γ_P^L and in Γ_O^R, and only the right one
       plays it. *)
    shared_left: table; (* Γ_O^R level ↦ Γ_P^L level *)
    shared_right: table; (* Γ_O^L level ↦ Γ_P^R level *)
    (* The P- and O-sides of E_L and E_R, the composite's position being the
       second participant: an exported name is a P-name of both component and
       composite, an Opponent-introduced one an O-name of both. *)
    externalLP: table; (* composite P level ↦ Γ_P^L level *)
    externalLO: table; (* Γ_O^L level ↦ composite O level *)
    externalRP: table; (* composite P level ↦ Γ_P^R level *)
    externalRO: table; (* Γ_O^R level ↦ composite O level *)
  }

  let empty =
    {
      shared_left= Util.Namespan.empty_nspan;
      shared_right= Util.Namespan.empty_nspan;
      externalLP= Util.Namespan.empty_nspan;
      externalLO= Util.Namespan.empty_nspan;
      externalRP= Util.Namespan.empty_nspan;
      externalRO= Util.Namespan.empty_nspan;
    }

  let pp fmt sync_state =
    let pp_table = Util.Namespan.pp_namespan Renaming.Namectx.Names.pp_name in
    Format.fprintf fmt "@[⟨Shared_LR: %a ∣ %a; E_L: %a ∣ %a; E_R: %a ∣ %a⟩@]"
      pp_table sync_state.shared_left pp_table sync_state.shared_right pp_table
      sync_state.externalLP pp_table sync_state.externalLO pp_table
      sync_state.externalRP pp_table sync_state.externalRO

  let to_yojson sync_state =
    let table_to_yojson =
      Util.Namespan.namespan_to_yojson Renaming.Namectx.Names.name_to_yojson
    in
    `Assoc
      [
        ("sharedLeft", table_to_yojson sync_state.shared_left);
        ("sharedRight", table_to_yojson sync_state.shared_right);
        ("externalLP", table_to_yojson sync_state.externalLP);
        ("externalLO", table_to_yojson sync_state.externalLO);
        ("externalRP", table_to_yojson sync_state.externalRP);
        ("externalRO", table_to_yojson sync_state.externalRO);
      ]

  (* The subject of a P-move of component [side] as the other component names
     it, or None when the subject is not shared. *)
  let lookup_shared sync_state side nn =
    match side with
    | Left -> Util.Pmap.lookup nn sync_state.shared_right
    | Right -> Util.Pmap.lookup nn sync_state.shared_left

  (* The external subject of a component's P-move, as the composite names it
     on its O-side. *)
  let lookup_externalO sync_state side nn =
    match side with
    | Left -> Util.Pmap.lookup nn sync_state.externalLO
    | Right -> Util.Pmap.lookup nn sync_state.externalRO

  (* The addressed component and its own name for the subject of an Opponent
     move, which is a P-name of the composite. *)
  let lookup_externalP sync_state nn =
    match Util.Pmap.lookup nn sync_state.externalLP with
    | Some level -> Some (Left, level)
    | None ->
        Option.map
          (fun level -> (Right, level))
          (Util.Pmap.lookup nn sync_state.externalRP)

  (* Append to [table] the zip of the two renamings placing a transmitted
     move's lnamectx, which share it as their domain (the lockstep
     invariant). *)
  (* A renaming need not send lnamectx to a contiguous block: in an aggregate
     context one mixing name kinds is split across the sub-contexts. *)
  let extend_table table dom_renaming codom_renaming =
    let lnamectx = Renaming.Namectx.get_names (Renaming.dom dom_renaming) in
    assert (lnamectx = Renaming.Namectx.get_names (Renaming.dom codom_renaming));
    Util.Pmap.concat
      (Util.Namespan.combine
         ( List.map (Renaming.lookup dom_renaming) lnamectx,
           List.map (Renaming.lookup codom_renaming) lnamectx ))
      table

  (* After a synchronization played by [side], which placed lnamectx into its
     namectxP while the other component placed it into its namectxO. *)
  let extend_shared sync_state side p_renaming o_renaming =
    match side with
    | Left ->
        {
          sync_state with
          shared_left= extend_table sync_state.shared_left o_renaming p_renaming;
        }
    | Right ->
        {
          sync_state with
          shared_right=
            extend_table sync_state.shared_right o_renaming p_renaming;
        }

  (* After an export, which placed lnamectx into the component's namectxP and
     into the composite's. *)
  let extend_externalP sync_state side component_renaming composite_renaming =
    match side with
    | Left ->
        {
          sync_state with
          externalLP=
            extend_table sync_state.externalLP composite_renaming
              component_renaming;
        }
    | Right ->
        {
          sync_state with
          externalRP=
            extend_table sync_state.externalRP composite_renaming
              component_renaming;
        }

  (* After an import, which placed lnamectx into the composite's namectxO and
     into the addressed component's. *)
  let extend_externalO sync_state side composite_renaming component_renaming =
    match side with
    | Left ->
        {
          sync_state with
          externalLO=
            extend_table sync_state.externalLO component_renaming
              composite_renaming;
        }
    | Right ->
        {
          sync_state with
          externalRO=
            extend_table sync_state.externalRO component_renaming
              composite_renaming;
        }

  (* Record one pair of the initial sharing, at the arbitrary names the
     initial contexts give the shared entity; None if either is already
     paired. *)
  let add_pair table dom_name codom_name =
    if
      Util.Pmap.mem dom_name table
      || List.mem codom_name (Util.Pmap.codom table)
    then None
    else Some (Util.Pmap.add (dom_name, codom_name) table)
end
