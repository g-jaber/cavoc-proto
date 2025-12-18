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
open Js_of_ocaml_lwt
open Lwt.Infix

let rec init_page () =
  Help_modal.init_help_events ();
  Printexc.record_backtrace true;
  let button = Dom_html.getElementById "submit" in
  let select_button = Dom_html.getElementById "select-btn" in
  let stop_button = Dom_html.getElementById "stop-btn" in

  Js.Unsafe.set select_button "disabled" Js._true;
  Js.Unsafe.set select_button "style"
    (Js.string "background-color: grey; cursor: not-allowed;");
  Js.Unsafe.set select_button "title"
    (Js.string "You must be evaluating code to select an move");
  
  Js.Unsafe.set stop_button "disabled" Js._true;
  Js.Unsafe.set stop_button "style"
    (Js.string "background-color: grey; cursor: not-allowed;");
  Js.Unsafe.set stop_button "title"
    (Js.string "You must be evaluating code to select an move");
  
  Js.Unsafe.set button "disabled" Js._false;
  Js.Unsafe.set button "style"
    (Js.string "background-color: ''; cursor: pointer;");
  Js.Unsafe.set button "title"
    (Js.string "Stop evaluation to evaluate new code");
  
  Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
      let%lwt _ = Js_of_ocaml_lwt.Lwt_js_events.click button in
      
      Js.Unsafe.set button "disabled" Js._true;
      Js.Unsafe.set button "style"
        (Js.string "background-color: grey; cursor: not-allowed;");
      Js.Unsafe.set button "title"
        (Js.string "Stop evaluation to evaluate new code");

      Js.Unsafe.set select_button "disabled" Js._false;
      Js.Unsafe.set select_button "style"
        (Js.string "background-color: ''; cursor: pointer;");
      Js.Unsafe.set select_button "title"
        (Js.string "You must be evaluating code to select an move");

      Js.Unsafe.set stop_button "disabled" Js._false;
      Js.Unsafe.set stop_button "style"
        (Js.string "background-color: ''; cursor: pointer;");
      Js.Unsafe.set stop_button "title"
        (Js.string "You must be evaluating code to select an move");
      
      Lwt.catch
        (fun () ->
        let%lwt result = Evaluate_code.evaluate_code () in match result with
          | 0 -> Lwt.fail (Failure "Stop")
          | 1 -> Lwt.fail (Failure "Stop")
          | _ -> Lwt.return_unit)
        (function
          | Failure msg when msg = "Stop" ->
              Js.Unsafe.set select_button "disabled" Js._true;
              Js.Unsafe.set select_button "style"
                (Js.string "background-color: grey; cursor: not-allowed;");
              Js.Unsafe.set select_button "title"
                (Js.string "You must be evaluating code to select an move");

              Js.Unsafe.set stop_button "disabled" Js._true;
              Js.Unsafe.set stop_button "style"
                (Js.string "background-color: grey; cursor: not-allowed;");
              Js.Unsafe.set stop_button "title"
                (Js.string "You must be evaluating code to select an move");

              init_page ();
              Lwt.return_unit
          | exn ->
              init_page ();
              Ui_helpers.print_to_output ("Unhandled exception: " ^ Printexc.to_string exn);
              Lwt.return_unit))
