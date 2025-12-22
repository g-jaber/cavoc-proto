open Js_of_ocaml

let () =
  Printexc.record_backtrace true;
  Sys_js.set_channel_flusher stdout Ui_helpers.print_to_output;
  Sys_js.set_channel_flusher stderr Ui_helpers.print_to_output;
  Page_init.init_page ()
