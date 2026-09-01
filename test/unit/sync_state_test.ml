(* Unit tests of the synchronization-state bookkeeping of the open
   composition (lib/lts/sync_state.ml), driving the seed → sync →
   dispatch → forward scenario of doc/compose.md's micro-trace. *)

module TestNames =
  Lang.Names.MakeInt
    (struct
      let is_callable = true
      let is_cname = false
    end)
    (struct
      let prefix = "n"
    end)
    ()

module TestNamectx =
  Lang.Typectx.Make_List
    (TestNames)
    (struct
      type t = string

      let to_yojson s = `String s
      let pp = Format.pp_print_string
    end)

module TestWeak = Lang.Renaming.MakeWeak (TestNamectx)
module SyncState = Lts.Sync_state.Make (TestWeak)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAILED: %s\n" name
  end

(* A plain context with n entries, all of the same type. *)
let make_ctx n =
  List.fold_left
    (fun ctx _ -> snd (TestNamectx.add_fresh ctx "" "τ"))
    TestNamectx.empty
    (List.init n Fun.id)

let delta1 = make_ctx 1

let () =
  (* Seeding and dispatch: Shared_LR seeded with one name provided by L,
     sitting at level 0 of Γ_P^L and level 0 of Γ_O^R. *)
  let s0 = SyncState.empty in
  let s1 =
    match
      SyncState.seed_pair s0.shared_left ~key_name:0
        ~value_name:0
    with
    | Some table -> { s0 with shared_left= table }
    | None -> failwith "seeding failed" in
  check "R addresses the seeded shared name"
    (SyncState.forward_subject s1 R 0 = Some 0);
  check "L does not know a shared name it did not receive"
    (SyncState.forward_subject s1 L 0 = None);

  (* Duplicate seeding is refused, on either column. *)
  check "seed_pair refuses a duplicate key level"
    (SyncState.seed_pair s1.shared_left ~key_name:0
       ~value_name:5
    = None);
  check "seed_pair refuses a duplicate value level"
    (SyncState.seed_pair s1.shared_left ~key_name:5
       ~value_name:0
    = None);

  (* The micro-trace synchronization: R calls the shared name with
     Δ = [c]. R places Δ into Γ_P^R (1 entry so far), L receives it into
     Γ_O^L (3 entries so far, its external names). *)
  let s2 =
    SyncState.extend_shared s1 ~side:R
      ~p_placement:(TestWeak.weak_r delta1 (make_ctx 1))
      ~o_placement:(TestWeak.weak_r delta1 (make_ctx 3)) in
  check "L addresses the forwarded name at its own level"
    (SyncState.forward_subject s2 L 3 = Some 1);
  check "levels below the extension stay unknown to L"
    (SyncState.forward_subject s2 L 2 = None);
  check "the seeded pair is untouched by the extension"
    (SyncState.forward_subject s2 R 0 = Some 0);

  (* Empty Δ: the extension is a no-op. *)
  let s3 =
    SyncState.extend_shared s2 ~side:L
      ~p_placement:(TestWeak.weak_r TestNamectx.empty (make_ctx 1))
      ~o_placement:(TestWeak.weak_r TestNamectx.empty (make_ctx 1))
  in
  check "empty Δ extends nothing" (s3 = s2);

  (* Export path: L's three external O-names correspond to the
     outer O-levels 0, 1, 2. *)
  let s4 =
    List.fold_left
      (fun sync_state level ->
        match
          SyncState.seed_pair sync_state.SyncState.externalL_opponent
            ~key_name:level ~value_name:level
        with
        | Some table ->
            { sync_state with SyncState.externalL_opponent= table }
        | None -> failwith "external seeding failed")
      s3 [ 0; 1; 2 ] in
  check "export translates an external subject"
    (SyncState.export_subject s4 L 1 = Some 1);
  check "export does not translate a shared subject"
    (SyncState.export_subject s4 L 3 = None);

  (* Import extension: the Opponent introduces one name to L, placed
     at outer O-level 3 and at level 4 of Γ_O^L (3 external names + 1 shared
     name already there). *)
  let s5 =
    SyncState.extend_external_opponent s4 ~side:L
      ~outer_placement:(TestWeak.weak_r delta1 (make_ctx 3))
      ~component_placement:(TestWeak.weak_r delta1 (make_ctx 4)) in
  check "the imported name is export-translatable afterwards"
    (SyncState.export_subject s5 L 4 = Some 3);

  (* Export extension and import lookup: L exports a P-name (level 1 of
     Γ_P^L, outer P-level 0), R exports one (level 2 of Γ_P^R, outer
     P-level 1). *)
  let s6 =
    SyncState.extend_external_proponent s5 ~side:L
      ~component_placement:(TestWeak.weak_r delta1 (make_ctx 1))
      ~outer_placement:(TestWeak.weak_r delta1 TestNamectx.empty) in
  let s7 =
    SyncState.extend_external_proponent s6 ~side:R
      ~component_placement:(TestWeak.weak_r delta1 (make_ctx 2))
      ~outer_placement:(TestWeak.weak_r delta1 (make_ctx 1)) in
  check "import selects the left component"
    (SyncState.import_subject s7 0 = Some (SyncState.L, 1));
  check "import selects the right component"
    (SyncState.import_subject s7 1 = Some (SyncState.R, 2));
  check "import rejects an unknown outer level"
    (SyncState.import_subject s7 2 = None);

  (* Coverage: the levels of Γ_O^L (now 5 entries) are partitioned
     between the shared and the external tables keyed by them. *)
  let shared_keys = Util.Pmap.dom s7.shared_right in
  let external_keys = Util.Pmap.dom s7.externalL_opponent in
  let all_keys = List.sort compare (shared_keys @ external_keys) in
  check "Γ_O^L is covered by exactly one table per level"
    (all_keys = [ 0; 1; 2; 3; 4 ]);

  (* Printers. *)
  check "pp produces output"
    (String.length (Format.asprintf "%a" SyncState.pp s7) > 0);
  check "namespan_to_yojson serializes pairs"
    (Util.Namespan.namespan_to_yojson
       (fun i -> `Int i)
       s7.shared_right
    = `List [ `List [ `Int 3; `Int 1 ] ]);
  (match SyncState.to_yojson s7 with
  | `Assoc fields -> check "to_yojson has the six tables" (List.length fields = 6)
  | _ -> check "to_yojson shape" false);

  if !failures = 0 then print_endline "sync_state: all tests passed"
  else begin
    Printf.eprintf "sync_state: %d test(s) failed\n" !failures;
    exit 1
  end
