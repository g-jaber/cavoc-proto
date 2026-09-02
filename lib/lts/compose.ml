(* Open composition of two components, σ_L ∥ σ_R = trace (σ_L ⊗ σ_R) over
   dynamic name contexts. *)

module type INT_STRUCTURE = sig
  (* Typing LTS of the components, shared by both: in v1, the standard
     instance. *)
  module TypingLTS : Typing.LTS

  (* Typing LTS of the composite, over the external contexts E_L and E_R,
     the only names the composite's Opponent ever sees. *)
  module CompositeTypingLTS :
    Typing.LTS with module BranchMonad = TypingLTS.BranchMonad

  type side = Left | Right

  (* The synchronization state: for each of E_L, Shared_LR and E_R, the span
     embedding that context into the contexts of the two that play on it. *)
  type t

  val pp_sync_state : Format.formatter -> t -> unit
  val sync_state_to_yojson : t -> Yojson.Safe.t

  (* Whether a P-move's subject is shared with the other component, read off
     the side's leg of the Shared_LR span; a name belongs to one context,
     fixed at its introduction. *)
  val is_shared_subject :
    t -> side -> TypingLTS.Moves.Renaming.Namectx.Names.name -> bool

  (* Translate a move of component [side] whose subject is external into the
     corresponding move of the composite, extending the external span. *)
  val export_move :
    t ->
    side ->
    CompositeTypingLTS.position ->
    TypingLTS.Moves.pol_move ->
    (CompositeTypingLTS.Moves.pol_move * t, string) result

  (* Inverse translation, for Opponent moves already accepted by
     [CompositeTypingLTS.check_move], selecting the component the move addresses. *)
  (* Errors on a move mentioning a name of another context, which the
     composite typing cannot detect. *)
  val import_move :
    t ->
    TypingLTS.position ->
    TypingLTS.position ->
    CompositeTypingLTS.Moves.pol_move ->
    (side * TypingLTS.Moves.pol_move * t, string) result

  (* Forward a move through Shared_LR to the other, passive component, with
     the direction switched and both legs of the span extended. *)
  (* The rest of the a_nf travels untouched, its non-subject names being
     move-local. *)
  val forward_move :
    t ->
    side ->
    TypingLTS.position ->
    TypingLTS.Moves.pol_move ->
    (TypingLTS.Moves.pol_move * t, string) result

  (* Which of one component's initial O-names the other provides, the rest
     going to the external contexts. *)
  type initial_sharing

  val initialize : initial_sharing -> t * CompositeTypingLTS.position
end

module Make
    (IntStructure : INT_STRUCTURE)
    (LeftComponent : Strategy.LTS with module TypingLTS = IntStructure.TypingLTS)
    (RightComponent :
      Strategy.LTS
        with module TypingLTS = IntStructure.TypingLTS
         and module EvalMonad = LeftComponent.EvalMonad) =
struct
  module EvalMonad = LeftComponent.EvalMonad

  type components =
    | ActiveLeft of LeftComponent.active_conf * RightComponent.passive_conf
    | ActiveRight of LeftComponent.passive_conf * RightComponent.active_conf

  type active_conf = {
    comps: components;
    pos: IntStructure.CompositeTypingLTS.position;
    sync_state: IntStructure.t;
  }

  type passive_conf = {
    pasL: LeftComponent.passive_conf;
    pasR: RightComponent.passive_conf;
    pos: IntStructure.CompositeTypingLTS.position;
    sync_state: IntStructure.t;
  }

  let passive_conf_to_yojson (pconf : passive_conf) =
    `Assoc
      [
        ("left", LeftComponent.passive_conf_to_yojson pconf.pasL);
        ("right", RightComponent.passive_conf_to_yojson pconf.pasR);
        ("pos", IntStructure.CompositeTypingLTS.position_to_yojson pconf.pos);
        ("syncState", IntStructure.sync_state_to_yojson pconf.sync_state);
      ]

  let pp_active_conf fmt (aconf : active_conf) =
    match aconf.comps with
    | ActiveLeft (actL, pasR) ->
        Format.fprintf fmt "@[⟨%a@ ∥@ %a@ @[Pos: %a@]@ @[Sync: %a@]⟩@]"
          LeftComponent.pp_active_conf actL RightComponent.pp_passive_conf pasR
          IntStructure.CompositeTypingLTS.pp_position aconf.pos
          IntStructure.pp_sync_state aconf.sync_state
    | ActiveRight (pasL, actR) ->
        Format.fprintf fmt "@[⟨%a@ ∥@ %a@ @[Pos: %a@]@ @[Sync: %a@]⟩@]"
          LeftComponent.pp_passive_conf pasL RightComponent.pp_active_conf actR
          IntStructure.CompositeTypingLTS.pp_position aconf.pos
          IntStructure.pp_sync_state aconf.sync_state

  let pp_passive_conf fmt (pconf : passive_conf) =
    Format.fprintf fmt "@[⟨%a@ ∥@ %a@ @[Pos: %a@]@ @[Sync: %a@]⟩@]"
      LeftComponent.pp_passive_conf pconf.pasL RightComponent.pp_passive_conf
      pconf.pasR IntStructure.CompositeTypingLTS.pp_position pconf.pos
      IntStructure.pp_sync_state pconf.sync_state

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let equiv_act_conf (a : active_conf) (b : active_conf) =
    match (a.comps, b.comps) with
    | (ActiveLeft (actL, _), ActiveLeft (actL', _)) ->
        LeftComponent.equiv_act_conf actL actL'
    | (ActiveRight (_, actR), ActiveRight (_, actR')) ->
        RightComponent.equiv_act_conf actR actR'
    | _ -> false

  let get_active_pos (aconf : active_conf) = aconf.pos
  let get_passive_pos (pconf : passive_conf) = pconf.pos

  (* The composite's init, from the two initial component configurations and
     the explicit initial sharing. *)
  let init_pconf (sharing : IntStructure.initial_sharing)
      (pasL : LeftComponent.passive_conf) (pasR : RightComponent.passive_conf) :
      passive_conf =
    let (sync_state, pos) = IntStructure.initialize sharing in
    (* TODO: debug-assert the sharing against the components'
       positions. *)
    { pasL; pasR; pos; sync_state }

  (* The same init with one side already active: a composition with an empty
     external Opponent context has no one to open the play. *)
  let init_aconf (sharing : IntStructure.initial_sharing) (comps : components) :
      active_conf =
    let (sync_state, pos) = IntStructure.initialize sharing in
    { comps; pos; sync_state }

  (* The par layer, one move per transition with synchronizations visible, on
     which the interactive explorer runs. *)
  (* It is no Strategy.LTS over the composite typing: sync moves are not
     typable there. *)
  module Par = struct
    type comp_move =
      | ExternalMove of IntStructure.CompositeTypingLTS.Moves.pol_move
      | SyncMove of IntStructure.TypingLTS.Moves.pol_move

    let string_of_comp_move = function
      | ExternalMove m ->
          IntStructure.CompositeTypingLTS.Moves.string_of_pol_move m
      | SyncMove m ->
          "τ[" ^ IntStructure.TypingLTS.Moves.string_of_pol_move m ^ "]"

    (* The display form of a comp_move played from [aconf], free names shown
       as the sender names them. *)
    (* A synchronization is an Output of its sender and an Input of its
       receiver at once, so it is displayed unpolarized. *)
    let string_of_comp_move_from (aconf : active_conf) = function
      | ExternalMove m ->
          let show_name =
            IntStructure.CompositeTypingLTS.Moves.Renaming.Namectx.show_name_in
              (IntStructure.CompositeTypingLTS.get_namectxO aconf.pos) in
          IntStructure.CompositeTypingLTS.Moves.string_of_pol_move_in ~show_name
            m
      | SyncMove (_, move) ->
          let sender_namectxO =
            match aconf.comps with
            | ActiveLeft (actL, _) ->
                IntStructure.TypingLTS.get_namectxO
                  (LeftComponent.get_active_pos actL)
            | ActiveRight (_, actR) ->
                IntStructure.TypingLTS.get_namectxO
                  (RightComponent.get_active_pos actR) in
          let show_name =
            IntStructure.TypingLTS.Moves.Renaming.Namectx.show_name_in
              sender_namectxO in
          IntStructure.TypingLTS.Moves.string_of_move_in ~show_name move

    type conf = Active of active_conf | Passive of passive_conf

    (* A free name besides the subject means the composition was applied
       outside the disclosure-free fragment. *)
    let free_name_error side m msg =
      failwith
        ("Composition outside the disclosure-free fragment: the move "
        ^ IntStructure.TypingLTS.Moves.string_of_pol_move m
        ^ " of the "
        ^ (match side with
          | IntStructure.Left -> "left"
          | IntStructure.Right -> "right")
        ^ " component: " ^ msg)

    (* One P-step of the active component, forwarded or exported according to
       whether the subject of the produced move is shared. *)
    let p_trans (aconf : active_conf) : (comp_move * conf) EvalMonad.m =
      let open EvalMonad in
      let route_move side m sync_state pos passive_pos emit sync =
        let (_, move) = m in
        let nn = IntStructure.TypingLTS.Moves.get_subject_name move in
        match IntStructure.is_shared_subject sync_state side nn with
        | false -> begin
            match IntStructure.export_move sync_state side pos m with
            | Error msg -> free_name_error side m msg
            | Ok (composite_move, sync_state) ->
                (* [trigger_move] trusts the weakening carried by the move,
                   which [export_move] has just built. *)
                let pos =
                  IntStructure.CompositeTypingLTS.trigger_move pos
                    composite_move in
                return (ExternalMove composite_move, emit sync_state pos)
          end
        | true -> begin
            match IntStructure.forward_move sync_state side passive_pos m with
            | Error msg -> free_name_error side m msg
            | Ok (forwarded_m, sync_state) -> begin
                match sync forwarded_m with
                | None ->
                    failwith
                      ("The synchronization move "
                      ^ IntStructure.TypingLTS.Moves.string_of_pol_move
                          forwarded_m
                      ^ " was refused by the passive component. Please report."
                      )
                | Some comps ->
                    return (SyncMove m, Active { comps; pos; sync_state })
              end
          end in
      match aconf.comps with
      | ActiveLeft (actL, pasR) ->
          let* (m, pasL) = LeftComponent.p_trans actL in
          route_move IntStructure.Left m aconf.sync_state aconf.pos
            (RightComponent.get_passive_pos pasR)
            (fun sync_state pos -> Passive { pasL; pasR; pos; sync_state })
            (fun forwarded_m ->
              Option.map
                (fun actR -> ActiveRight (pasL, actR))
                (RightComponent.o_trans pasR forwarded_m))
      | ActiveRight (pasL, actR) ->
          let* (m, pasR) = RightComponent.p_trans actR in
          route_move IntStructure.Right m aconf.sync_state aconf.pos
            (LeftComponent.get_passive_pos pasL)
            (fun sync_state pos -> Passive { pasL; pasR; pos; sync_state })
            (fun forwarded_m ->
              Option.map
                (fun actL -> ActiveLeft (actL, pasR))
                (LeftComponent.o_trans pasL forwarded_m))

    (* An Opponent move, checked at the composite typing then imported into
       the component it addresses, which re-checks it at the inner typing. *)
    (* A move outside the disclosure-free fragment is refused, like an
       ill-typed one. *)
    let o_trans (pconf : passive_conf)
        (composite_move : IntStructure.CompositeTypingLTS.Moves.pol_move) :
        active_conf option =
      match
        IntStructure.CompositeTypingLTS.check_move pconf.pos composite_move
      with
      | None -> None
      | Some pos -> begin
          match
            IntStructure.import_move pconf.sync_state
              (LeftComponent.get_passive_pos pconf.pasL)
              (RightComponent.get_passive_pos pconf.pasR)
              composite_move
          with
          | Error _ -> None
          | Ok (side, m, sync_state) -> begin
              match side with
              | IntStructure.Left ->
                  Option.map
                    (fun actL ->
                      { comps= ActiveLeft (actL, pconf.pasR); pos; sync_state })
                    (LeftComponent.o_trans pconf.pasL m)
              | IntStructure.Right ->
                  Option.map
                    (fun actR ->
                      { comps= ActiveRight (pconf.pasL, actR); pos; sync_state })
                    (RightComponent.o_trans pconf.pasR m)
            end
        end

    (* The Opponent of the composite plays on the external contexts, which
       its own typing describes, so its moves are generated there and o_trans
       then routes each into the component it addresses. *)
    (* Generating them at the components instead would need the subject
       translated towards the Opponent, the one direction the spans do not
       run. *)
    let o_trans_gen (pconf : passive_conf) :
        (IntStructure.CompositeTypingLTS.Moves.pol_move * active_conf)
        IntStructure.CompositeTypingLTS.BranchMonad.m =
      let open IntStructure.CompositeTypingLTS.BranchMonad in
      let* (composite_move, _) =
        IntStructure.CompositeTypingLTS.generate_moves pconf.pos in
      match o_trans pconf composite_move with
      | None -> fail ()
      | Some aconf -> return (composite_move, aconf)
  end

  (* The hide layer, the trace operator proper, iterating the par layer until
     an external move appears. *)
  (* Infinite chattering is divergence of the composite, witnessed only by a
     fuel bound (not yet implemented). *)
  module Hide :
    Strategy.LTS
      with module TypingLTS = IntStructure.CompositeTypingLTS
       and module EvalMonad = LeftComponent.EvalMonad
       and type active_conf = active_conf
       and type passive_conf = passive_conf = struct
    module TypingLTS = IntStructure.CompositeTypingLTS
    module EvalMonad = EvalMonad

    type nonrec active_conf = active_conf
    type nonrec passive_conf = passive_conf

    let passive_conf_to_yojson = passive_conf_to_yojson

    type conf = Active of active_conf | Passive of passive_conf

    let string_of_active_conf = string_of_active_conf
    let string_of_passive_conf = string_of_passive_conf
    let pp_active_conf = pp_active_conf
    let pp_passive_conf = pp_passive_conf
    let equiv_act_conf = equiv_act_conf
    let get_active_pos = get_active_pos
    let get_passive_pos = get_passive_pos

    let rec p_trans (aconf : active_conf) :
        (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.m =
      let open EvalMonad in
      let* (cm, next) = Par.p_trans aconf in
      match (cm, next) with
      | (Par.ExternalMove m, Par.Passive pconf) -> return (m, pconf)
      | (Par.SyncMove _, Par.Active aconf) -> p_trans aconf (* chattering *)
      | (Par.ExternalMove _, Par.Active _) | (Par.SyncMove _, Par.Passive _) ->
          assert false

    let o_trans = Par.o_trans
    let o_trans_gen = Par.o_trans_gen
  end
end

(* The v1 instance of INT_STRUCTURE: disclosure-free composition, with
   Sync_state as synchronization state. *)
(* Nothing here assumes how the components represent names: the spans relate
   whatever names the two participants' contexts give a shared entity. *)
module MakeIntStructure
    (IntLang : Lang.Interactive.LANG)
    (TypingLTS :
      Typing.LTS
        with module Moves.Renaming = IntLang.IEnv.Renaming
         and type Moves.copattern =
          IntLang.abstract_normal_form * IntLang.IEnv.Renaming.t) =
struct
  module TypingLTS = TypingLTS
  module CompositeTypingLTS = TypingLTS
  module SyncState = Sync_state.Make (TypingLTS.Moves.Renaming)
  module Namectx = IntLang.IEnv.Renaming.Namectx

  type side = SyncState.side = Left | Right
  type t = SyncState.t

  let pp_sync_state = SyncState.pp
  let sync_state_to_yojson = SyncState.to_yojson

  let is_shared_subject sync_state side nn =
    Option.is_some (SyncState.lookup_shared sync_state side nn)

  (* A transmitted a_nf mentions no free name besides its subject: the
     disclosure-free fragment. *)
  let check_free_names a_nf =
    let subject = IntLang.get_subject_name a_nf in
    let offenders =
      IntLang.fold_free_names_of_a_nf
        (fun offenders nn ->
          if nn = subject then offenders else nn :: offenders)
        [] a_nf in
    match offenders with
    | [] -> Ok ()
    | _ ->
        Error
          ("its abstract normal form mentions the ambient name(s) "
          ^ String.concat ", " (List.map Namectx.Names.string_of_name offenders)
          ^ " besides its subject")

  (* After check_free_names, the subject is the only free name of the a_nf, so
     mapping the free names translates exactly it. *)
  let translate_subject a_nf subject translated_subject =
    IntLang.map_free_names_of_a_nf
      (fun nn ->
        assert (nn = subject);
        translated_subject)
      a_nf

  let forward_move sync_state side passive_pos
      ((dir, (a_nf, sender_renaming)) : TypingLTS.Moves.pol_move) =
    assert (dir = TypingLTS.Moves.Output);
    let nn = IntLang.get_subject_name a_nf in
    match check_free_names a_nf with
    | Error msg -> Error msg
    | Ok () -> begin
        match SyncState.lookup_shared sync_state side nn with
        | None ->
            failwith
              "Forwarding a move whose subject is not shared. Please report."
        | Some forwarded_subject ->
            let ((_, forwarded_renaming) as forwarded_move) =
              TypingLTS.weaken_move passive_pos TypingLTS.Moves.Input
                (translate_subject a_nf nn forwarded_subject, sender_renaming)
            in
            let sync_state =
              SyncState.extend_shared sync_state side sender_renaming
                forwarded_renaming in
            Ok ((TypingLTS.Moves.Input, forwarded_move), sync_state)
      end

  let export_move sync_state side composite_pos
      ((dir, (a_nf, component_renaming)) : TypingLTS.Moves.pol_move) =
    assert (dir = TypingLTS.Moves.Output);
    let nn = IntLang.get_subject_name a_nf in
    match check_free_names a_nf with
    | Error msg -> Error msg
    | Ok () -> begin
        match SyncState.lookup_externalO sync_state side nn with
        | None ->
            failwith
              "Exporting a move whose subject is not external. Please report."
        | Some composite_level ->
            let ((_, composite_renaming) as composite_move) =
              CompositeTypingLTS.weaken_move composite_pos
                CompositeTypingLTS.Moves.Output
                (translate_subject a_nf nn composite_level, component_renaming)
            in
            let sync_state =
              SyncState.extend_externalP sync_state side component_renaming
                composite_renaming in
            Ok ((CompositeTypingLTS.Moves.Output, composite_move), sync_state)
      end

  let import_move sync_state left_pos right_pos
      ((dir, (a_nf, composite_renaming)) : CompositeTypingLTS.Moves.pol_move) =
    assert (dir = CompositeTypingLTS.Moves.Input);
    let nn = IntLang.get_subject_name a_nf in
    match check_free_names a_nf with
    | Error msg -> Error msg
    | Ok () -> begin
        match SyncState.lookup_externalP sync_state nn with
        | None ->
            failwith
              "Importing an Opponent move with a subject unknown to both \
               components. Please report."
        | Some (side, component_level) ->
            let component_pos =
              match side with Left -> left_pos | Right -> right_pos in
            let ((_, component_renaming) as component_move) =
              TypingLTS.weaken_move component_pos TypingLTS.Moves.Input
                (translate_subject a_nf nn component_level, composite_renaming)
            in
            let sync_state =
              SyncState.extend_externalO sync_state side composite_renaming
                component_renaming in
            Ok (side, (TypingLTS.Moves.Input, component_move), sync_state)
      end

  (* The name pairs initialize Shared_LR, the remaining names going to the external
     contexts, the left component's before the right one's. *)
  type initial_sharing = {
    left_init_pos: TypingLTS.position;
    right_init_pos: TypingLTS.position;
    provided_by_left: (Namectx.Names.name * Namectx.Names.name) list;
        (* (Γ_P^L name, Γ_O^R name) *)
    provided_by_right: (Namectx.Names.name * Namectx.Names.name) list;
        (* (Γ_P^R name, Γ_O^L name) *)
    initial_storectx: TypingLTS.store_ctx;
  }

  let initialize sharing =
    let namectxP_left = TypingLTS.get_namectxP sharing.left_init_pos in
    let namectxO_left = TypingLTS.get_namectxO sharing.left_init_pos in
    let namectxP_right = TypingLTS.get_namectxP sharing.right_init_pos in
    let namectxO_right = TypingLTS.get_namectxO sharing.right_init_pos in
    let add_pair table dom_name codom_name =
      match SyncState.add_pair table dom_name codom_name with
      | Some table -> table
      | None -> failwith "Ill-formed initial sharing: a name is paired twice."
    in
    let add_shared namectxP namectxO table (p_name, o_name) =
      assert (
        Namectx.lookup_exn namectxP p_name = Namectx.lookup_exn namectxO o_name);
      add_pair table o_name p_name in
    let shared_left =
      List.fold_left
        (add_shared namectxP_left namectxO_right)
        SyncState.empty.shared_left sharing.provided_by_left in
    let shared_right =
      List.fold_left
        (add_shared namectxP_right namectxO_left)
        SyncState.empty.shared_right sharing.provided_by_right in
    let external_names namectx shared_names =
      List.filter
        (fun nn -> not (List.mem nn shared_names))
        (Namectx.get_names namectx) in
    (* One component's external names on one polarity: extend the composite
       context and the corresponding table. *)
    (* A P-table has the composite name in its domain, an O-table in its
       codomain. *)
    let add_entryP table composite_name nn = add_pair table composite_name nn in
    let add_entryO table composite_name nn = add_pair table nn composite_name in
    let external_block add_entry (composite_namectx, table) component_namectx
        names =
      List.fold_left
        (fun (composite_namectx, table) nn ->
          let ty = Namectx.lookup_exn component_namectx nn in
          let str = Namectx.show_name_in component_namectx nn in
          let (composite_name, composite_namectx) =
            Namectx.add_fresh composite_namectx str ty in
          (composite_namectx, add_entry table composite_name nn))
        (composite_namectx, table) names in
    let (composite_namectxP, externalLP) =
      external_block add_entryP
        (Namectx.empty, SyncState.empty.externalLP)
        namectxP_left
        (external_names namectxP_left (List.map fst sharing.provided_by_left))
    in
    let (composite_namectxP, externalRP) =
      external_block add_entryP
        (composite_namectxP, SyncState.empty.externalRP)
        namectxP_right
        (external_names namectxP_right (List.map fst sharing.provided_by_right))
    in
    let (composite_namectxO, externalLO) =
      external_block add_entryO
        (Namectx.empty, SyncState.empty.externalLO)
        namectxO_left
        (external_names namectxO_left (List.map snd sharing.provided_by_right))
    in
    let (composite_namectxO, externalRO) =
      external_block add_entryO
        (composite_namectxO, SyncState.empty.externalRO)
        namectxO_right
        (external_names namectxO_right (List.map snd sharing.provided_by_left))
    in
    let sync_state =
      {
        SyncState.shared_left;
        shared_right;
        externalLP;
        externalLO;
        externalRP;
        externalRO;
      } in
    let pos =
      CompositeTypingLTS.init_pas_pos sharing.initial_storectx
        composite_namectxP composite_namectxO in
    (sync_state, pos)
end
