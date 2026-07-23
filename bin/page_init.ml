(* ==========================================
   PAGE_INIT: Page initialization and control
   ==========================================
   Initializes the web page and manages UI state:
   - init_page: Main entry point (recursive function)
     * Sets up button event handlers
     * Manages button enabled/disabled states
     * Calls evaluate_code when submit button is clicked
     * Handles move selection and next step functionality
   - Coordinates the entire interactive workflow
*)

open Js_of_ocaml

(* Shown on the Evaluate button while an interaction is running. The buttons
   that need the opposite condition use Ui_helpers' default title. *)
let idle_title = "Stop the current interaction before evaluating new code"

(* The buttons that only make sense while an interaction is running. *)
let interaction_buttons = [ "select-btn"; "stop-btn" ]

(* The buttons used to navigate between the configurations of a symbolic LTS. *)
let configuration_buttons = [ "conf-prev"; "conf-next"; "conf-accept" ]

let set_all_enabled ?title ids state =
  List.iter (fun id -> Ui_helpers.set_button_enabled ?title id state) ids

(* Outside of an interaction the user may only start one; while one is running
   the reverse holds. *)
let set_interaction_running running =
  set_all_enabled interaction_buttons running;
  Ui_helpers.set_button_enabled ~title:idle_title "submit" (not running);
  (* The options select which LTS to explore, so they only take effect at the
     next evaluation. Disable them during a run rather than let the user change
     them to no visible effect. *)
  Ui_helpers.set_button_enabled ~title:idle_title "options-btn" (not running)

(* Shown once the server has been shut down, since the page can no longer do
   anything useful. *)
let show_server_stopped () =
  let overlay = Dom_html.createDiv Dom_html.document in
  overlay##.id := Js.string "server-stopped";
  overlay##.textContent :=
    Js.some
      (Js.string
         "Server stopped. Close this tab, or restart the server with \
          \"dune exec ./bin/server.exe\".");
  Dom.appendChild Dom_html.document##.body overlay

let init_shutdown_button () =
  match Dom_html.getElementById_opt "shutdown-btn" with
  | None -> ()
  | Some shut_button ->
      shut_button##.onclick :=
        Dom_html.handler (fun _ ->
            (* Shutting down is irreversible from the page, so confirm first. *)
            if
              Js.to_bool
                (Dom_html.window##confirm
                   (Js.string
                      "Shut down the server? The page will stop working."))
            then
              Lwt.async (fun () ->
                  let%lwt _ = Js_of_ocaml_lwt.XmlHttpRequest.get "../stop" in
                  show_server_stopped ();
                  Lwt.return_unit);
            Js._true)

let init_debug_checkbox () =
  match Dom_html.getElementById_opt "debug-log-check" with
  | None -> ()
  | Some debug_btn ->
      Js.Opt.iter (Dom_html.CoerceTo.input debug_btn) (fun input ->
          input##.checked := Js.bool !Util.Debug.debug_mode)

let rec init_page () =
  Help_modal.init_help_events ();
  Printexc.record_backtrace true;
  init_shutdown_button ();
  init_debug_checkbox ();

  set_all_enabled configuration_buttons false;
  set_interaction_running false;

  match Dom_html.getElementById_opt "submit" with
  | None -> ()
  | Some submit_button ->
      Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
          let%lwt _ = Js_of_ocaml_lwt.Lwt_js_events.click submit_button in
          set_interaction_running true;

          (* The interaction reports why it stopped, including the user leaving
             the game, so there is nothing to catch here beyond genuine
             failures. Either way the page returns to its idle state and waits
             for the next evaluation. *)
          let%lwt () =
            Lwt.catch
              (fun () -> Evaluate_code.evaluate_code ())
              (fun exn ->
                Ui_helpers.print_to_output
                  ("Unhandled exception: " ^ Printexc.to_string exn);
                Ui_helpers.show_tab "console";
                Lwt.return_unit) in

          set_interaction_running false;
          init_page ();
          Lwt.return_unit)
