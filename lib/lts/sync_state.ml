(* Synchronization state of the disclosure-free (v1) open composition
   (doc/compose.md): the three interface spans — E_L, Shared_LR, E_R —
   each stored as its two polarized correspondence tables. A table is a
   namespan pairing the names a shared entity has in the contexts of the
   two participants playing on the interface — plain levels when the
   contexts are plain, but nothing here depends on that. A table is keyed by the copy
   at the participant that plays moves on the name — the one that did
   not introduce it, since subjects are O-names — and valued in the copy
   at the introducing side, so each table is consulted in exactly one
   direction. *)

module Make (Renaming : Lang.Renaming.WEAKENING) = struct
  type side = L | R
  type table = Renaming.Namectx.Names.name Util.Namespan.namespan

  type t = {
    (* The two halves of Shared_LR, by introducing side: a name
       introduced by L sits at its P-level in Γ_P^L and at its O-level
       in Γ_O^R, and only R plays it. *)
    shared_left: table; (* Γ_O^R level ↦ Γ_P^L level *)
    shared_right: table; (* Γ_O^L level ↦ Γ_P^R level *)
    (* The P- and O-halves of E_L and E_R, the composite's outer
       position being the second participant: a component's exported
       name is a P-name of both the component and the composite, an
       Opponent-introduced name an O-name of both. *)
    externalL_proponent: table; (* outer P level ↦ Γ_P^L level *)
    externalL_opponent: table; (* Γ_O^L level ↦ outer O level *)
    externalR_proponent: table; (* outer P level ↦ Γ_P^R level *)
    externalR_opponent: table; (* Γ_O^R level ↦ outer O level *)
  }

  let empty =
    {
      shared_left= Util.Namespan.empty_nspan;
      shared_right= Util.Namespan.empty_nspan;
      externalL_proponent= Util.Namespan.empty_nspan;
      externalL_opponent= Util.Namespan.empty_nspan;
      externalR_proponent= Util.Namespan.empty_nspan;
      externalR_opponent= Util.Namespan.empty_nspan;
    }

  let pp fmt sync_state =
    let pp_table =
      Util.Namespan.pp_namespan Renaming.Namectx.Names.pp_name in
    Format.fprintf fmt
      "@[⟨Shared_LR: %a ∣ %a; E_L: %a ∣ %a; E_R: %a ∣ %a⟩@]" pp_table
      sync_state.shared_left pp_table
      sync_state.shared_right pp_table
      sync_state.externalL_proponent pp_table
      sync_state.externalL_opponent pp_table
      sync_state.externalR_proponent pp_table
      sync_state.externalR_opponent

  let to_yojson sync_state =
    let table_to_yojson =
      Util.Namespan.namespan_to_yojson Renaming.Namectx.Names.name_to_yojson in
    `Assoc
      [
        ( "sharedLeft",
          table_to_yojson sync_state.shared_left );
        ( "sharedRight",
          table_to_yojson sync_state.shared_right );
        ( "externalLProponent",
          table_to_yojson sync_state.externalL_proponent );
        ( "externalLOpponent",
          table_to_yojson sync_state.externalL_opponent );
        ( "externalRProponent",
          table_to_yojson sync_state.externalR_proponent );
        ( "externalROpponent",
          table_to_yojson sync_state.externalR_opponent );
      ]

  (* Dispatch and subject translation, fused, for a P-move of component
     side: a Some result means the subject is on the shared interface
     and gives its P-level at the other component. *)
  let forward_subject sync_state side nn =
    match side with
    | L -> Util.Pmap.lookup nn sync_state.shared_right
    | R -> Util.Pmap.lookup nn sync_state.shared_left

  (* Export: the external subject of a component's P-move, translated to
     its level on the composite's outer O-side. *)
  let export_subject sync_state side nn =
    match side with
    | L -> Util.Pmap.lookup nn sync_state.externalL_opponent
    | R -> Util.Pmap.lookup nn sync_state.externalR_opponent

  (* Import: the subject of an Opponent move is a P-name of the
     composite; selects the addressed component and its level for it. *)
  let import_subject sync_state nn =
    match Util.Pmap.lookup nn sync_state.externalL_proponent with
    | Some level -> Some (L, level)
    | None ->
        Option.map
          (fun level -> (R, level))
          (Util.Pmap.lookup nn sync_state.externalR_proponent)

  (* extend_table table ~keys ~values appends the zip of the two
     placements of a transmitted move's Δ: ~keys places Δ in the context
     the table is keyed by, ~values in the one it is valued in. The two
     placements share their domain — the move-local Δ, in the same order
     (the lockstep invariant) — so pairing them is a pointwise walk of
     that domain through each placement.
     Nothing here assumes where a placement sends Δ: in an aggregate
     context a Δ mixing name kinds is split across the sub-contexts, so
     the placed names form no contiguous block. *)
  let extend_table table ~keys ~values =
    let delta = Renaming.Namectx.get_names (Renaming.dom keys) in
    assert (delta = Renaming.Namectx.get_names (Renaming.dom values));
    Util.Pmap.concat
      (Util.Namespan.combine
         ( List.map (Renaming.lookup keys) delta,
           List.map (Renaming.lookup values) delta ))
      table

  (* After a synchronization played by ~side: it placed Δ into its
     P-context (~p_placement), the other component into its O-context
     (~o_placement). *)
  let extend_shared sync_state ~side ~p_placement
      ~o_placement =
    match side with
    | L ->
        {
          sync_state with
          shared_left=
            extend_table sync_state.shared_left
              ~keys:o_placement ~values:p_placement;
        }
    | R ->
        {
          sync_state with
          shared_right=
            extend_table sync_state.shared_right
              ~keys:o_placement ~values:p_placement;
        }

  (* After an export: the component placed Δ into its P-context, the
     composite into its outer P-context. *)
  let extend_external_proponent sync_state ~side ~component_placement
      ~outer_placement =
    match side with
    | L ->
        {
          sync_state with
          externalL_proponent=
            extend_table sync_state.externalL_proponent
              ~keys:outer_placement ~values:component_placement;
        }
    | R ->
        {
          sync_state with
          externalR_proponent=
            extend_table sync_state.externalR_proponent
              ~keys:outer_placement ~values:component_placement;
        }

  (* After an import: the Opponent placed Δ into the outer O-context,
     the addressed component into its O-context. *)
  let extend_external_opponent sync_state ~side
      ~outer_placement ~component_placement =
    match side with
    | L ->
        {
          sync_state with
          externalL_opponent=
            extend_table sync_state.externalL_opponent
              ~keys:component_placement ~values:outer_placement;
        }
    | R ->
        {
          sync_state with
          externalR_opponent=
            extend_table sync_state.externalR_opponent
              ~keys:component_placement ~values:outer_placement;
        }

  (* seed_pair table ~key_name ~value_name records one pair of the
     initial sharing, at the arbitrary names the initial contexts give
     the shared entity; None if either name is already paired. *)
  let seed_pair table ~key_name ~value_name =
    if
      Util.Pmap.mem key_name table
      || List.mem value_name (Util.Pmap.codom table)
    then None
    else Some (Util.Pmap.add (key_name, value_name) table)
end
