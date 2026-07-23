open Js_of_ocaml

let set_debug enabled =
  let open Util.Debug in
  debug_mode := enabled

let () =
  Printexc.record_backtrace true;
  Sys_js.set_channel_flusher stdout Ui_helpers.print_to_output;
  (* Anything written to stderr is an error worth seeing, so bring the Console
     tab forward when it happens. *)
  Sys_js.set_channel_flusher stderr (fun s ->
      Ui_helpers.print_to_output s;
      Ui_helpers.show_tab "console");
  Js.export "cavocToggleDebug" set_debug ;
  Page_init.init_page ()
