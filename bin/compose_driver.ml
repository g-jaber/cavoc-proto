(* =====================================================
   COMPOSE_DRIVER: the par-layer interaction loop
   =====================================================
   Drives the open composition of two modules at its par layer (through
   Ogs.Compose_lts): one exchange per step, so the internal chattering is
   shown instead of being iterated away by the trace operator, and the
   page stays responsive while the components talk. Synchronizations are
   not choices — they auto-play under the chattering control of the
   shared gutter (auto with a delay, or step by step), so a diverging
   chatter stays visible and interruptible; the user is only consulted
   at external moves, through the same choice panel as the single-module
   interaction.

   This is the front-end driver of doc/web.md (stage 2). It exists
   because Lts.Interactive_build is written over Strategy.LTS, which the
   par layer is not (sync moves are not typable at the composite's outer
   typing); what it needed from the engine — the τ tag, the display of a
   move at its sender's naming, one exchange per transition — is the
   data for deciding whether comp_move should ever become a genuine
   Moves instance (doc/compose.md, open decision 1).
*)

open Js_of_ocaml
open Js_of_ocaml_lwt

(* What the chattering control (front/common.js, window.cavocChattering)
   says to do with the next synchronization. *)
type chattering_setting = Auto of float (* delay in seconds *) | Step

let chattering_setting () =
  let control : 'a Js.Optdef.t =
    Js.Unsafe.get Js.Unsafe.global (Js.string "cavocChattering") in
  Js.Optdef.case control
    (fun () -> Auto 0.4)
    (fun control ->
      let mode : Js.js_string Js.t Js.Optdef.t =
        Js.Unsafe.get control (Js.string "mode") in
      let is_step =
        Js.Optdef.case mode (fun () -> false) (fun m -> Js.to_string m = "step")
      in
      if is_step then Step
      else
        let delay_ms : float Js.Optdef.t =
          Js.Unsafe.get control (Js.string "delayMs") in
        Auto (Js.Optdef.case delay_ms (fun () -> 0.4) (fun ms -> ms /. 1000.)))

(* Waits for the chattering control to release the next synchronization:
   the delay in auto mode, the user in step mode — and Stop interrupts
   either wait. The setting is re-read after every wait, so flipping the
   control takes effect on the very next exchange.

   Releasing a synchronization is the user playing, so step mode asks for
   it through the choice panel like every other question, as its single
   row; auto mode replaces the rows with a note. 
   [rendered] is what keeps the pause from re-rendering on every re-read. *)
let rec pace_chattering ?(rendered = false) () : [ `Play | `Interrupted ] Lwt.t
    =
  let stop_click result =
    match Dom_html.getElementById_opt "stop-btn" with
    | None -> []
    | Some button ->
        [ (let%lwt _ = Lwt_js_events.click button in Lwt.return result) ] in
  match chattering_setting () with
  | Auto delay ->
      if not rendered then
        Moves_display.render_notice ~caption:"The components are chattering."
          "Options ▾ switches to step to release \
           them one at a time.";
      let%lwt outcome =
        Lwt.pick
          ((let%lwt () = Lwt_js.sleep delay in Lwt.return `Play)
           :: stop_click `Interrupted) in
      Lwt.return outcome
  | Step ->
      if not rendered then
        Moves_display.render_choice_list ~caption:"Chattering paused"
          [ (0, "next ▸ play one synchronization") ];
      (* The wait also times out to re-read the setting, so switching
         from step back to auto releases a pending pause. *)
      let%lwt outcome =
        Lwt.pick
          ((let%lwt () = Lwt_js.sleep 0.25 in Lwt.return `Recheck)
           :: (let%lwt chosen = Moves_display.wait_choice () in
               Lwt.return
                 (match chosen with
                 | Interaction_signals.Chosen _ -> `Play
                 | Interaction_signals.Interrupted
                 | Interaction_signals.Missing_buttons ->
                     `Interrupted))
           :: stop_click `Interrupted) in
      (match outcome with
      | `Recheck -> (
          match chattering_setting () with
          | Step -> pace_chattering ~rendered:true ()
          | Auto _ -> Lwt.return `Play)
      | (`Play | `Interrupted) as decided -> Lwt.return decided)

module Drive (Composition : Lts_kind.SINGLE_RESULT_COMPOSITION_WITH_INIT) =
struct
  let show_name_at namectx =
    Composition.TypingLTS.Moves.Renaming.Namectx.show_name_in namectx

  (* The interaction loop, mirroring Lts.Interactive_build with the
     active branch split by the kind of comp_move: a sync move is shown
     on the shared lane and auto-played at the chattering pace, an
     external move is shown on the public lane, and only a passive
     composite asks the user. show_moves_list and get_move are
     Evaluate_code's own callbacks, passed in so the choice panel
     behaves exactly as in single mode. *)
  let rec drive ~show_moves_list ~get_move (conf : Composition.conf) :
      Lts.Interactive_build.outcome Lwt.t =
    let open Lts.Interactive_build in
    match conf with
    | Composition.Active aconf -> begin
        match Composition.EvalMonad.run (Composition.par_p_trans aconf) with
        | Composition.EvalMonad.PropStop -> Lwt.return Prop_stopped
        | Composition.EvalMonad.PropDiverges -> Lwt.return Prop_diverges
        | Composition.EvalMonad.Continue (comp_move, next) ->
            let move_string =
              Composition.string_of_comp_move_from aconf comp_move in
            if Composition.comp_move_is_sync comp_move then begin
              (* The pace gates the synchronization being revealed, so in
                 step mode nothing appears until the user releases it; the
                 pause owns the choice panel while it waits. *)
              let%lwt pace = pace_chattering () in
              match pace with
              | `Interrupted ->
                  Moves_manager.clear_list ();
                  Lwt.return User_quit
              | `Play ->
                  Moves_manager.add_sync_move move_string;
                  drive ~show_moves_list ~get_move next
            end
            else begin
              Moves_manager.add_move Proponent move_string;
              drive ~show_moves_list ~get_move next
            end
      end
    | Composition.Passive pconf ->
        Display_config.display_composite_conf
          (Composition.passive_conf_to_yojson pconf);
        let show_name =
          show_name_at
            (Composition.TypingLTS.get_namectxP
               (Composition.get_passive_pos pconf)) in
        let results_list =
          Composition.TypingLTS.BranchMonad.run
            (Composition.o_trans_gen pconf) in
        let moves_list = List.map (fun (x, _) -> x) results_list in
        let json_list =
          List.map
            (Composition.TypingLTS.Moves.pol_move_to_yojson_in ~show_name)
            moves_list in
        show_moves_list json_list;
        let%lwt chosen = get_move (List.length json_list - 1) in
        (match chosen with
        | Quit -> Lwt.return User_quit
        | Chose chosen_index ->
            let (input_move, aconf) = List.nth results_list chosen_index in
            let move_string =
              Composition.TypingLTS.Moves.string_of_pol_move_in ~show_name
                input_move in
            Moves_manager.add_move Opponent move_string;
            drive ~show_moves_list ~get_move (Composition.Active aconf))
end
