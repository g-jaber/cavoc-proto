(* =========================================
   EVALUATE CODE: Main interactive evaluation loop
   =========================================
   Orchestrates the complete evaluation workflow:
   - evaluate_code: Main entry point that:
     * Fetches code from editors
     * Initializes LTS with chosen configuration
     * Coordinates callbacks for displaying moves,
       configuration, and move history
     * Implements the step-by-step evaluation loop
   - Integrates all other modules to provide full
     interactive evaluation experience
*)

open Js_of_ocaml

module MyLwt = struct
  type 'a m = 'a Lwt.t

  let return = Lwt.return
  let ( let* ) = Lwt.bind
end

let show_move player move = Moves_manager.add_move player move
let show_conf conf : unit = Display_config.display_conf conf

(* Reports why the interaction stopped. Leaving the game is the user's own
   doing, so it is only logged; the other outcomes are worth a modal. *)
let report_interaction_end (outcome : Lts.Interactive_build.outcome) =
  let open Lts.Interactive_build in
  match outcome with
  | User_quit ->
      Ui_helpers.print_to_output (string_of_outcome outcome);
      Lwt.return_unit
  | Prop_stopped | Prop_diverges ->
      let text =
        match outcome with
        (* In RefML, Proponent stops playing exactly when an assertion
           failed. *)
        | Prop_stopped -> "The module has produced a failed assertion."
        | _ -> string_of_outcome outcome in
      let message = Js.string text in
      let () = Js.Unsafe.global##onSuccess (Js.Unsafe.inject message) in
      Lwt.return_unit

let show_moves_list (json_list : Yojson.Safe.t list) =
  let display_of (v : Yojson.Safe.t) = Yojson.Safe.pretty_to_string v in
  let id_of i (v : Yojson.Safe.t) =
    match v with
    | `Assoc fields -> (
        match List.assoc_opt "id" fields with Some (`Int n) -> n | _ -> i)
    | _ -> i in
  let moves = List.mapi (fun i v -> (id_of i v, display_of v)) json_list in
  Moves_display.generate_clickables moves

let get_move n =
  let open Interaction_signals in
  let nb_moves = n + 1 in
  let%lwt choice = Moves_display.get_chosen_move () in
  match choice with
  | Chosen i when i >= 0 && i < nb_moves ->
      Ui_helpers.print_to_output ("Selected move: " ^ string_of_int i);
      Lwt.return (Lts.Interactive_build.Chose i)
  | Chosen i ->
      Ui_helpers.print_to_output
        (Printf.sprintf "Move index %d out of range (0..%d)" i (nb_moves - 1));
      Lwt.fail (Failure "Move index out of range")
  | Interrupted ->
      Moves_manager.clear_list ();
      Lwt.return Lts.Interactive_build.Quit
  | Missing_buttons as choice ->
      let msg = string_of_move_choice choice in
      Ui_helpers.print_to_output ("Error: " ^ msg);
      Lwt.fail (Failure msg)

(* What to show for one of the configurations the user is browsing. *)
(* Terminal branches carry their own explanation, so that Proponent stopping
   and the program diverging stay distinguishable. *)
type conf_preview =
  | Runnable of Yojson.Safe.t  (** the passive configuration to display *)
  | Terminal of string  (** why this branch has no continuation *)

(* Choosing between the configurations a symbolic evaluation split into goes
   through the same panel as the moves. *)
(* Focusing a row previews the configuration it stands for, clicking it
   continues there. *)
let choose_conf confs =
  let open Lts.Interactive_build in
  let nconf = List.length confs in

  (* avoid prompting the user everytime the LTS stops *)
  if nconf = 1 then Lwt.return (Chose 0)
  else
    match Dom_html.getElementById_opt "moves-list" with
    | None ->
        (* This page offers no choice panel, so waiting for a click that
           cannot happen would hang the interaction. *)
        Ui_helpers.print_to_output
          (Printf.sprintf
             "No choice panel on this page: selecting the first of %d \
              configurations."
             nconf);
        Lwt.return (Chose 0)
    | Some _ -> (
        let label i = function
          | Runnable _ ->
              Printf.sprintf "Continue with configuration %d of %d" (i + 1)
                nconf
          | Terminal reason ->
              Printf.sprintf "Configuration %d of %d — %s" (i + 1) nconf reason
        in
        (* Browsing must not advance the Δ baselines: each browsed
           configuration diffs against the one before the split. *)
        let preview i =
          match List.nth_opt confs i with
          | Some (Runnable conf) ->
              Display_config.display_conf ~preview:true conf
          | Some (Terminal reason) ->
              Display_config.display_terminal_conf reason
          | None -> () in
        Moves_display.render_choice_list
          ~caption:"The evaluation split — choose a configuration"
          ~on_focus:preview
          (List.mapi (fun i conf -> (i, label i conf)) confs);
        let%lwt choice = Moves_display.wait_choice () in
        let open Interaction_signals in
        match choice with
        | Chosen i when i >= 0 && i < nconf -> Lwt.return (Chose i)
        | Chosen i ->
            Ui_helpers.print_to_output
              (Printf.sprintf "Configuration index %d out of range (0..%d)" i
                 (nconf - 1));
            Lwt.fail (Failure "Configuration index out of range")
        | Interrupted -> Lwt.return Quit
        | Missing_buttons -> Lwt.return (Chose 0))

module RunMultiLts (MultiLts : Lts_kind.MULTI_RESULT_LTS_WITH_INIT) = struct
  include MultiLts
  module UserMonad = MyLwt

  let choose m =
    let open UserMonad in
    let open Lts.Interactive_build in
    let res = EvalMonad.run m in
    let res_to_preview = function
      | EvalMonad.PropStop ->
          Terminal "An assertion fails in this configuration."
      | EvalMonad.PropDiverges ->
          Terminal "The program diverges in this configuration."
      | EvalMonad.Continue (_, pas_conf) ->
          Runnable (passive_conf_to_yojson pas_conf) in
    let* choosen_conf = choose_conf (List.map res_to_preview res) in
    match choosen_conf with
    | Quit -> return Quit
    | Chose i -> return (Chose (List.nth res i))

  let _ = choose
end

module RunSingleLts (SingleLts : Lts_kind.SINGLE_RESULT_LTS_WITH_INIT) = struct
  include SingleLts
  module UserMonad = MyLwt

  let choose m =
    let open Lts.Interactive_build in
    UserMonad.return (Chose (EvalMonad.run m))
end

(* A lexing buffer on a card's editor content, named so the lexer reports
   errors at the participant's file name. *)
let participant_lexbuf participant ~ext text =
  let lexbuf = Lexing.from_string text in
  Lexing.set_filename lexbuf (participant.Editor_manager.participant_name ^ ext);
  lexbuf

(* The open composition of the first two cards, provider then client. *)
(* It runs on the concrete stack, driven at its par layer so the internal
   chattering shows. *)
let evaluate_composition kind_lts provider client =
  let (module Composition) = Lts_kind.build_compose_lts kind_lts in
  let module Driver = Compose_driver.Drive (Composition) in
  let init_conf =
    Composition.Passive
      (Composition.lexing_init_pconf
         ~provider_implem:
           (participant_lexbuf provider ~ext:".ml"
              provider.Editor_manager.module_code)
         ~provider_sig:
           (participant_lexbuf provider ~ext:".mli"
              provider.Editor_manager.signature_code)
         ~client_implem:
           (participant_lexbuf client ~ext:".ml"
              client.Editor_manager.module_code)
         ~client_sig:
           (participant_lexbuf client ~ext:".mli"
              client.Editor_manager.signature_code)
           (* A second buffer on the provider's signature, which is read
              twice: as the provider's exports, then as the client's
              imports. *)
         ~imported_sig:
           (participant_lexbuf provider ~ext:".mli"
              provider.Editor_manager.signature_code)) in
  let%lwt outcome = Driver.drive ~show_moves_list ~get_move init_conf in
  report_interaction_end outcome

(* The synthesis page, where only the first card's signature is read and both
   participants of the play are shown in the two cards. *)
let evaluate_synthesis participant =
  let (module Arena) = Lts_kind.build_arena () in
  let module Driver = Synthesis_driver.Drive (Arena) in
  let position =
    Arena.initial_position
      (participant_lexbuf participant ~ext:".mli"
         participant.Editor_manager.signature_code) in
  let show_play played =
    Synthesis_display.display_synthesized_participants
      ~module_source:(fun () -> Arena.synthesize_module_source position played)
      ~client_source:(fun () -> Arena.synthesize_client_source position played)
  in
  let%lwt outcome =
    Driver.drive ~show_moves_list ~get_move ~show_play ~arena:position position
      [] in
  match outcome with
  | Lts.Interactive_build.Prop_stopped ->
      (* The arena, not a program, is what has run out of moves here. *)
      Ui_helpers.print_to_output "The arena offers no move at this position.";
      Lwt.return_unit
  | outcome -> report_interaction_end outcome

let evaluate_code () =
  Moves_manager.flush_moves ();
  Synthesis_display.reset ();
  Display_config.reset_move_deltas ();

  (* Single-module exploration runs over the leftmost card, a compose scenario
     over the first two, provider then client. *)
  match
    (Lts_config.scenario_mode (), Editor_manager.fetch_participant_sources ())
  with
  | (_, []) ->
      Ui_helpers.print_to_output "No participant card on this page.";
      Ui_helpers.show_tab "console";
      Lwt.return_unit
  | ("compose", provider :: client :: _) ->
      Synthesis_display.display_no_client
        "Client synthesis is not available for a composition.";
      evaluate_composition (Lts_config.generate_kind_lts ()) provider client
  | ("compose", [ _ ]) ->
      Ui_helpers.print_to_output
        "A compose scenario needs two participant cards.";
      Ui_helpers.show_tab "console";
      Lwt.return_unit
  | ("synthesis", participant :: _) ->
      Synthesis_display.reset_participants ();
      Synthesis_display.display_no_client
        "On this page the client is a card of its own.";
      evaluate_synthesis participant
  | (_, participant :: _) ->
      let kind_lts = Lts_config.generate_kind_lts () in

      let lexBuffer_code =
        Lexing.from_string participant.Editor_manager.module_code in
      let lexBuffer_sig =
        Lexing.from_string participant.Editor_manager.signature_code in

      Lexing.set_filename lexBuffer_code
        (participant.Editor_manager.participant_name ^ ".ml");
      Lexing.set_filename lexBuffer_sig
        (participant.Editor_manager.participant_name ^ ".mli");

      if kind_lts.Lts_kind.symbolic then (
        let (module OGS_LTS) = Lts_kind.build_symbolic_lts kind_lts in
        let module RunLts = RunMultiLts (OGS_LTS) in
        let module IBuild = Lts.Interactive_build.Make (MyLwt) (RunLts) in
        let init_conf =
          OGS_LTS.Passive
            (OGS_LTS.lexing_init_pconf lexBuffer_code lexBuffer_sig) in

        Synthesis_display.display_no_client
          "Client synthesis is not available in the symbolic setting.";
        let%lwt outcome =
          IBuild.interactive_build ~show_move ~show_conf ~show_moves_list
            ~get_move init_conf in
        report_interaction_end outcome)
      else
        let (module OGS_LTS) = Lts_kind.build_concrete_lts kind_lts in
        let module RunLts = RunSingleLts (OGS_LTS) in
        let module IBuild = Lts.Interactive_build.Make (MyLwt) (RunLts) in
        let init_pconf =
          OGS_LTS.lexing_init_pconf lexBuffer_code lexBuffer_sig in

        (* Refreshed at the user's moves only: a Proponent move leaves the client
       unchanged, the synthesis dropping the response it has not answered. *)
        let initial_position = OGS_LTS.get_passive_pos init_pconf in
        let recorded_moves = ref [] in
        let record_move =
          match OGS_LTS.synthesize_client_source with
          | None ->
              Synthesis_display.display_no_client
                "Client synthesis is available in the CPS setting only.";
              fun _ -> ()
          | Some synthesize_client_source ->
              fun move ->
                recorded_moves := move :: !recorded_moves;
                if fst move = OGS_LTS.TypingLTS.Moves.Input then
                  Synthesis_display.display_synthesized_client
                    ~imports:participant.Editor_manager.signature_code
                    (fun () ->
                      synthesize_client_source initial_position
                        (List.rev !recorded_moves)) in

        let%lwt outcome =
          IBuild.interactive_build ~record_move ~show_move ~show_conf
            ~show_moves_list ~get_move (OGS_LTS.Passive init_pconf) in
        report_interaction_end outcome
