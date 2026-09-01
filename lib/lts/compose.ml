(* Open composition of two components: the parallel composition of the
   Functorial OGS project, σ_L ∥ σ_R = trace (σ_L ⊗ σ_R), over dynamic
   interfaces. Design note: doc/compose.md.

   The scheduler (Make below) is written once against INT_STRUCTURE,
   which packages everything representation-specific: dispatch on the
   interface the subject of a move lives on, translation of moves between
   component and composite contexts, forwarding of moves between the two
   components, the crossing check, and the composite's own typing LTS.
   The Echo of the Int-construction — a pure strategy in Functorial OGS —
   is not an operation here: it is realized by the synchronization branch
   of the scheduler together with the synchronization state threaded
   through these operations. The v1 instance keeps spans of thinnings
   placing the three interface contexts (E_L, Shared_LR, E_R) into the
   participants' own contexts; v2 extends forwarding with transport of
   free names for disclosure, leaving Make unchanged. *)

module type INT_STRUCTURE = sig
  (* Typing LTS of the components, shared by both: in v1, the standard
     instance. *)
  module Inner : Typing.LTS

  (* Typing LTS of the composite, over the external interfaces E_L and
     E_R: the only interface the composite's Opponent ever sees. Names
     of Shared_LR never appear in its positions or moves. In v1 this is
     the same instance as Inner. *)
  module Outer : Typing.LTS with module BranchMonad = Inner.BranchMonad

  type side = L | R

  (* The interface of the composite on which a name is used, fixed at
     its introduction: the external interface (E_L or E_R, played with
     the composite's Opponent) or the shared interface Shared_LR between
     the two components. *)
  type interface = ExternalInterface | SharedInterface

  (* The synchronization state: for each of the three interface contexts
     (E_L, Shared_LR, E_R), the span of thinnings placing it into the
     contexts of the two participants playing on it — component and
     composite for E_L/E_R, the two components for Shared_LR. A move
     crossing an interface has its subject translated along the partial
     renaming induced by the interface's span; the two legs of a span
     always share their domain (the lockstep invariant). *)
  type t

  val pp_sync_state : Format.formatter -> t -> unit
  val sync_state_to_yojson : t -> Yojson.Safe.t

  (* Dispatch: retraction of the subject against the side's leg of the
     Shared_LR span decides between the shared and the external
     interface. *)
  val interface_of_subject :
    t -> side -> Inner.Moves.Renaming.Namectx.Names.name -> interface

  (* Translate a move of component side whose subject is on the
     external interface into the corresponding move of the composite:
     subject translated along the external span, Δ re-placed by
     Outer.place at outer_position, span extended. Errors on
     crossing. *)
  val export_to_opponent :
    t ->
    side ->
    outer_position:Outer.position ->
    Inner.Moves.pol_move ->
    (Outer.Moves.pol_move * t, string) result

  (* Inverse translation, for Opponent moves accepted by
     Outer.check_move: selects the addressed component, translates the
     subject, re-places Δ at that component's position. Errors when the
     move is outside the disclosure-free fragment, which
     Outer.check_move cannot detect. *)
  val import_from_opponent :
    t ->
    left_position:Inner.position ->
    right_position:Inner.position ->
    Outer.Moves.pol_move ->
    (side * Inner.Moves.pol_move * t, string) result

  (* Forwarding across Shared_LR, to the other, passive component:
     subject translated along the span-induced renaming, direction
     switched, Δ re-placed by Inner.place at passive_position, both
     legs of the span extended. The rest of the a_nf travels untouched
     (its non-subject names are move-local). Errors on crossing. *)
  val forward_across_shared_interface :
    t ->
    side ->
    passive_position:Inner.position ->
    Inner.Moves.pol_move ->
    (Inner.Moves.pol_move * t, string) result

  (* The initial sharing: which of one component's initial O-names the
     other component provides, the rest going to the external
     interfaces. Seeds the synchronization state and the composite's
     initial position. *)
  type initial_sharing

  val initialize : initial_sharing -> t * Outer.position
end

module Make
    (I : INT_STRUCTURE)
    (L : Strategy.LTS with module TypingLTS = I.Inner)
    (R : Strategy.LTS
           with module TypingLTS = I.Inner
            and module EvalMonad = L.EvalMonad) =
struct
  module EvalMonad = L.EvalMonad

  type components =
    | ActiveLeft of L.active_conf * R.passive_conf
    | ActiveRight of L.passive_conf * R.active_conf

  type active_conf = {
    comps: components;
    pos: I.Outer.position;
    sync_state: I.t;
  }

  type passive_conf = {
    pasL: L.passive_conf;
    pasR: R.passive_conf;
    pos: I.Outer.position;
    sync_state: I.t;
  }

  let passive_conf_to_yojson (pconf : passive_conf) =
    `Assoc
      [
        ("left", L.passive_conf_to_yojson pconf.pasL);
        ("right", R.passive_conf_to_yojson pconf.pasR);
        ("pos", I.Outer.position_to_yojson pconf.pos);
        ("syncState", I.sync_state_to_yojson pconf.sync_state);
      ]

  let pp_active_conf fmt (aconf : active_conf) =
    match aconf.comps with
    | ActiveLeft (actL, pasR) ->
        Format.fprintf fmt "@[⟨%a@ ∥@ %a@ @[Pos: %a@]@ @[Sync: %a@]⟩@]"
          L.pp_active_conf actL R.pp_passive_conf pasR I.Outer.pp_position
          aconf.pos I.pp_sync_state aconf.sync_state
    | ActiveRight (pasL, actR) ->
        Format.fprintf fmt "@[⟨%a@ ∥@ %a@ @[Pos: %a@]@ @[Sync: %a@]⟩@]"
          L.pp_passive_conf pasL R.pp_active_conf actR I.Outer.pp_position
          aconf.pos I.pp_sync_state aconf.sync_state

  let pp_passive_conf fmt (pconf : passive_conf) =
    Format.fprintf fmt "@[⟨%a@ ∥@ %a@ @[Pos: %a@]@ @[Sync: %a@]⟩@]"
      L.pp_passive_conf pconf.pasL R.pp_passive_conf pconf.pasR
      I.Outer.pp_position pconf.pos I.pp_sync_state pconf.sync_state

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  (* TODO: the passive sides should be compared too, but Strategy.LTS
     exposes no equivalence on passive configurations. *)
  let equiv_act_conf (a : active_conf) (b : active_conf) =
    match (a.comps, b.comps) with
    | (ActiveLeft (actL, _), ActiveLeft (actL', _)) ->
        L.equiv_act_conf actL actL'
    | (ActiveRight (_, actR), ActiveRight (_, actR')) ->
        R.equiv_act_conf actR actR'
    | _ -> false

  let get_active_pos (aconf : active_conf) = aconf.pos
  let get_passive_pos (pconf : passive_conf) = pconf.pos

  (* The composite's init: the two initial component configurations
     plus the explicit initial sharing; the 3-lexbuffer front door is
     built on top of this. *)
  let init_pconf ~(sharing : I.initial_sharing) (pasL : L.passive_conf)
      (pasR : R.passive_conf) : passive_conf =
    let (sync_state, pos) = I.initialize sharing in
    (* TODO: debug-assert the sharing against the components'
       positions. *)
    { pasL; pasR; pos; sync_state }

  (* The par layer: one exchange per transition, synchronizations as
     visible transitions; the layer the interactive explorer runs. It is
     not a Strategy.LTS over the composite typing: sync moves are not
     typable there. *)
  module Par = struct
    type comp_move =
      | ExternalMove of I.Outer.Moves.pol_move
      | SyncMove of I.Inner.Moves.pol_move

    let string_of_comp_move = function
      | ExternalMove m -> I.Outer.Moves.string_of_pol_move m
      | SyncMove m -> "τ[" ^ I.Inner.Moves.string_of_pol_move m ^ "]"

    (* The display form of a comp_move played from [aconf]: free names are
       shown as the sender names them — the active component's O-context
       for a sync move, the composite's outer O-context for an external
       one (the subject of a P-move is an O-name of its sender). A
       synchronization is an Output of its sender and an Input of its
       receiver at once, so it is displayed unpolarized. *)
    let string_of_comp_move_from (aconf : active_conf) = function
      | ExternalMove m ->
          let show_name =
            I.Outer.Moves.Renaming.Namectx.show_name_in
              (I.Outer.get_namectxO aconf.pos) in
          I.Outer.Moves.string_of_pol_move_in ~show_name m
      | SyncMove (_, move) ->
          let sender_namectxO =
            match aconf.comps with
            | ActiveLeft (actL, _) ->
                I.Inner.get_namectxO (L.get_active_pos actL)
            | ActiveRight (_, actR) ->
                I.Inner.get_namectxO (R.get_active_pos actR) in
          let show_name =
            I.Inner.Moves.Renaming.Namectx.show_name_in sender_namectxO in
          I.Inner.Moves.string_of_move_in ~show_name move

    type conf = Active of active_conf | Passive of passive_conf

    (* A crossing move means the composition was applied outside the
       disclosure-free fragment: reject loudly. *)
    let crossing_error side m msg =
      failwith
        ("Composition outside the disclosure-free fragment: the move "
        ^ I.Inner.Moves.string_of_pol_move m
        ^ " of the "
        ^ (match side with I.L -> "left" | I.R -> "right")
        ^ " component crosses the shared interface: " ^ msg)

    (* One P-step of the active component, dispatched on the interface of
       the subject of the produced move. *)
    let p_trans (aconf : active_conf) : (comp_move * conf) EvalMonad.m =
      let open EvalMonad in
      let dispatch side m sync_state pos ~passive_position emit sync =
        let (_, (nn, _)) = m in
        match I.interface_of_subject sync_state side nn with
        | I.ExternalInterface -> begin
            match
              I.export_to_opponent sync_state side ~outer_position:pos m
            with
            | Error msg -> crossing_error side m msg
            | Ok (outer_m, sync_state) ->
                (* trigger_move trusts the placement carried by outer_m,
                   which export_to_opponent has just built through the
                   policy of I.Outer.place. *)
                let pos = I.Outer.trigger_move pos outer_m in
                return (ExternalMove outer_m, emit sync_state pos)
          end
        | I.SharedInterface -> begin
            match
              I.forward_across_shared_interface sync_state side
                ~passive_position m
            with
            | Error msg -> crossing_error side m msg
            | Ok (forwarded_m, sync_state) -> begin
                match sync forwarded_m with
                | None ->
                    failwith
                      ("The synchronization move "
                      ^ I.Inner.Moves.string_of_pol_move forwarded_m
                      ^ " was refused by the passive component. Please \
                         report.")
                | Some comps ->
                    return (SyncMove m, Active { comps; pos; sync_state })
              end
          end in
      match aconf.comps with
      | ActiveLeft (actL, pasR) ->
          let* (m, pasL) = L.p_trans actL in
          dispatch I.L m aconf.sync_state aconf.pos
            ~passive_position:(R.get_passive_pos pasR)
            (fun sync_state pos -> Passive { pasL; pasR; pos; sync_state })
            (fun forwarded_m ->
              Option.map
                (fun actR -> ActiveRight (pasL, actR))
                (R.o_trans pasR forwarded_m))
      | ActiveRight (pasL, actR) ->
          let* (m, pasR) = R.p_trans actR in
          dispatch I.R m aconf.sync_state aconf.pos
            ~passive_position:(L.get_passive_pos pasL)
            (fun sync_state pos -> Passive { pasL; pasR; pos; sync_state })
            (fun forwarded_m ->
              Option.map
                (fun actL -> ActiveLeft (actL, pasR))
                (L.o_trans pasL forwarded_m))

    (* An Opponent move, checked at the composite typing (the untrusted
       entry point), then imported into the component it addresses,
       whose o_trans re-checks it at the inner typing. A move outside
       the disclosure-free fragment is refused, like an ill-typed
       one. *)
    let o_trans (pconf : passive_conf) (outer_m : I.Outer.Moves.pol_move) :
        active_conf option =
      match I.Outer.check_move pconf.pos outer_m with
      | None -> None
      | Some pos -> begin
          match
            I.import_from_opponent pconf.sync_state
              ~left_position:(L.get_passive_pos pconf.pasL)
              ~right_position:(R.get_passive_pos pconf.pasR) outer_m
          with
          | Error _ -> None
          | Ok (side, m, sync_state) -> begin
              match side with
              | I.L ->
                  Option.map
                    (fun actL ->
                      { comps= ActiveLeft (actL, pconf.pasR); pos; sync_state })
                    (L.o_trans pconf.pasL m)
              | I.R ->
                  Option.map
                    (fun actR ->
                      { comps= ActiveRight (pconf.pasL, actR); pos; sync_state })
                    (R.o_trans pconf.pasR m)
            end
        end

    (* The Opponent of the composite plays on the external interfaces, which
       are what the composite's own typing describes, so its moves are
       generated there. Generating them at the components instead would need
       the subject translated from a component's context out to the
       Opponent's — the one direction the spans are not keyed for — and would
       range over the shared interface, on which the Opponent never plays.
       o_trans then routes each move into the component it addresses. *)
    let o_trans_gen (pconf : passive_conf) :
        (I.Outer.Moves.pol_move * active_conf) I.Outer.BranchMonad.m =
      let open I.Outer.BranchMonad in
      let* (outer_m, _) = I.Outer.generate_moves pconf.pos in
      match o_trans pconf outer_m with
      | None -> fail ()
      | Some aconf -> return (outer_m, aconf)
  end

  (* The hide layer: the trace operator proper, iterating the par layer
     until an external move appears. Infinite chattering is divergence
     of the composite, witnessed only by a fuel bound (not yet
     implemented). *)
  module Hide :
    Strategy.LTS
      with module TypingLTS = I.Outer
       and module EvalMonad = L.EvalMonad
       and type active_conf = active_conf
       and type passive_conf = passive_conf = struct
    module TypingLTS = I.Outer
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
      | (Par.ExternalMove _, Par.Active _) | (Par.SyncMove _, Par.Passive _)
        ->
          assert false

    let o_trans = Par.o_trans
    let o_trans_gen = Par.o_trans_gen
  end
end

(* The v1 instance of INT_STRUCTURE: disclosure-free composition, with
   Sync_state as synchronization state. Nothing here assumes how the
   components represent names: the spans relate whatever names the two
   participants' contexts give a shared entity. *)
module MakeIntStructure
    (IntLang : Lang.Interactive.LANG)
    (TypingLTS : Typing.LTS
                   with module Moves.Renaming = IntLang.IEnv.Renaming
                    and type Moves.copattern =
                     IntLang.abstract_normal_form * IntLang.IEnv.Renaming.t) =
struct
  module Inner = TypingLTS
  module Outer = TypingLTS
  module SyncState = Sync_state.Make (TypingLTS.Moves.Renaming)
  module Namectx = IntLang.IEnv.Renaming.Namectx

  type side = SyncState.side = L | R
  type interface = ExternalInterface | SharedInterface
  type t = SyncState.t

  let pp_sync_state = SyncState.pp
  let sync_state_to_yojson = SyncState.to_yojson

  let interface_of_subject sync_state side nn =
    match SyncState.forward_subject sync_state side nn with
    | Some _ -> SharedInterface
    | None -> ExternalInterface

  (* The crossing check of the disclosure-free fragment: a transmitted
     a_nf mentions no ambient name besides its subject. *)
  let check_no_crossing a_nf =
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

  (* After check_no_crossing, the subject is the only free name of the
     a_nf, so mapping the free names translates exactly it. *)
  let translate_subject a_nf ~subject ~translated_subject =
    IntLang.map_free_names_of_a_nf
      (fun nn ->
        assert (nn = subject);
        translated_subject)
      a_nf

  let forward_across_shared_interface sync_state side ~passive_position
      ((dir, (nn, (a_nf, sender_placement))) : Inner.Moves.pol_move) =
    assert (dir = Inner.Moves.Output);
    match check_no_crossing a_nf with
    | Error msg -> Error msg
    | Ok () -> begin
        match
          SyncState.forward_subject sync_state side nn
        with
        | None ->
            failwith
              "Forwarding a move whose subject is not on the shared \
               interface. Please report."
        | Some forwarded_subject ->
            let local_delta = Inner.Moves.Renaming.dom sender_placement in
            let forwarded_placement =
              Inner.place passive_position Inner.Moves.Input forwarded_subject
                local_delta in
            let sync_state =
              SyncState.extend_shared sync_state ~side
                ~p_placement:sender_placement ~o_placement:forwarded_placement
            in
            let a_nf =
              translate_subject a_nf ~subject:nn
                ~translated_subject:forwarded_subject in
            Ok
              ( (Inner.Moves.Input, (forwarded_subject, (a_nf, forwarded_placement))),
                sync_state )
      end

  let export_to_opponent sync_state side ~outer_position
      ((dir, (nn, (a_nf, component_placement))) : Inner.Moves.pol_move) =
    assert (dir = Inner.Moves.Output);
    match check_no_crossing a_nf with
    | Error msg -> Error msg
    | Ok () -> begin
        match
          SyncState.export_subject sync_state
            side nn
        with
        | None ->
            failwith
              "Exporting a move whose subject is not on the external \
               interface. Please report."
        | Some outer_level ->
            let local_delta = Inner.Moves.Renaming.dom component_placement in
            let outer_placement =
              Outer.place outer_position Outer.Moves.Output outer_level
                local_delta in
            let sync_state =
              SyncState.extend_external_proponent sync_state ~side
                ~component_placement ~outer_placement in
            let a_nf =
              translate_subject a_nf ~subject:nn
                ~translated_subject:outer_level in
            Ok
              ( (Outer.Moves.Output, (outer_level, (a_nf, outer_placement))),
                sync_state )
      end

  let import_from_opponent sync_state ~left_position ~right_position
      ((dir, (nn, (a_nf, outer_placement))) : Outer.Moves.pol_move) =
    assert (dir = Outer.Moves.Input);
    match check_no_crossing a_nf with
    | Error msg -> Error msg
    | Ok () -> begin
        match SyncState.import_subject sync_state nn with
        | None ->
            failwith
              "Importing an Opponent move with a subject unknown to both \
               components. Please report."
        | Some (side, component_level) ->
            let component_position =
              match side with L -> left_position | R -> right_position in
            let local_delta =
              Outer.Moves.Renaming.dom outer_placement in
            let component_placement =
              Inner.place component_position Inner.Moves.Input component_level
                local_delta in
            let sync_state =
              SyncState.extend_external_opponent sync_state ~side
                ~outer_placement ~component_placement in
            let a_nf =
              translate_subject a_nf ~subject:nn
                ~translated_subject:component_level in
            Ok
              ( side,
                (Inner.Moves.Input, (component_level, (a_nf, component_placement))),
                sync_state )
      end

  (* The name pairs seed Shared_LR, as (P-name at the providing side,
     O-name at the other) in a common order; the remaining names go to
     the external interfaces, L's before R's, in context order. *)
  type initial_sharing = {
    left_initial_position: Inner.position;
    right_initial_position: Inner.position;
    provided_by_left: (Namectx.Names.name * Namectx.Names.name) list;
        (* (Γ_P^L name, Γ_O^R name) *)
    provided_by_right: (Namectx.Names.name * Namectx.Names.name) list;
        (* (Γ_P^R name, Γ_O^L name) *)
    initial_storectx: Inner.store_ctx;
  }

  let initialize sharing =
    let namectxP_left = Inner.get_namectxP sharing.left_initial_position in
    let namectxO_left = Inner.get_namectxO sharing.left_initial_position in
    let namectxP_right = Inner.get_namectxP sharing.right_initial_position in
    let namectxO_right = Inner.get_namectxO sharing.right_initial_position in
    let seed table ~key_name ~value_name =
      match SyncState.seed_pair table ~key_name ~value_name with
      | Some table -> table
      | None ->
          failwith "Ill-formed initial sharing: a name is paired twice." in
    let seed_shared namectxP namectxO table (p_name, o_name) =
      assert (
        Namectx.lookup_exn namectxP p_name
        = Namectx.lookup_exn namectxO o_name);
      seed table ~key_name:o_name ~value_name:p_name in
    let shared_left =
      List.fold_left
        (seed_shared namectxP_left namectxO_right)
        SyncState.empty.shared_left sharing.provided_by_left
    in
    let shared_right =
      List.fold_left
        (seed_shared namectxP_right namectxO_left)
        SyncState.empty.shared_right sharing.provided_by_right
    in
    let external_names namectx shared_names =
      List.filter
        (fun nn -> not (List.mem nn shared_names))
        (Namectx.get_names namectx) in
    (* One component's external names on one polarity: extend the outer
       context and the corresponding table. *)
    let external_block ~outer_keyed (outer_namectx, table) component_namectx
        names =
      List.fold_left
        (fun (outer_namectx, table) nn ->
          let ty = Namectx.lookup_exn component_namectx nn in
          let str = Namectx.show_name_in component_namectx nn in
          let (outer_name, outer_namectx) =
            Namectx.add_fresh outer_namectx str ty in
          let table =
            if outer_keyed then
              seed table ~key_name:outer_name ~value_name:nn
            else seed table ~key_name:nn ~value_name:outer_name
          in
          (outer_namectx, table))
        (outer_namectx, table) names in
    let (outer_namectxP, externalL_proponent) =
      external_block ~outer_keyed:true
        (Namectx.empty, SyncState.empty.externalL_proponent)
        namectxP_left
        (external_names namectxP_left (List.map fst sharing.provided_by_left))
    in
    let (outer_namectxP, externalR_proponent) =
      external_block ~outer_keyed:true
        (outer_namectxP, SyncState.empty.externalR_proponent)
        namectxP_right
        (external_names namectxP_right
           (List.map fst sharing.provided_by_right)) in
    let (outer_namectxO, externalL_opponent) =
      external_block ~outer_keyed:false
        (Namectx.empty, SyncState.empty.externalL_opponent)
        namectxO_left
        (external_names namectxO_left (List.map snd sharing.provided_by_right))
    in
    let (outer_namectxO, externalR_opponent) =
      external_block ~outer_keyed:false
        (outer_namectxO, SyncState.empty.externalR_opponent)
        namectxO_right
        (external_names namectxO_right (List.map snd sharing.provided_by_left))
    in
    let sync_state =
      {
        SyncState.shared_left;
        shared_right;
        externalL_proponent;
        externalL_opponent;
        externalR_proponent;
        externalR_opponent;
      } in
    let pos =
      Outer.init_pas_pos sharing.initial_storectx outer_namectxP
        outer_namectxO in
    (sync_state, pos)
end
