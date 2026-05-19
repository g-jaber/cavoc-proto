open Js_of_ocaml

let set_debug enabled =
  let open Util.Debug in
  debug_mode := enabled

let () =
  Printexc.record_backtrace true;
  Sys_js.set_channel_flusher stdout Ui_helpers.print_to_output;
  Sys_js.set_channel_flusher stderr Ui_helpers.print_to_output;
  Js.export "cavocToggleDebug" set_debug ;
  Page_init.init_page ()
