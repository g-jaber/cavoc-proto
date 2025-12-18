(* ========================================
   UI_HELPERS: Low-level HTML/DOM utilities
   ========================================
   Provides basic functions for DOM manipulation and output handling:
   - print_to_output: Appends formatted text to the console div
   - update_container: Updates HTML content of a specific element
   - html_escape: Escapes special characters for safe HTML display
*)

open Js_of_ocaml

let print_to_output str =
  let output_div = Dom_html.getElementById "console" in
  let current_content = Js.to_string (Js.Unsafe.get output_div "innerHTML") in
  let new_content = current_content ^ "<pre>" ^ str ^ "</pre>" in
  Js.Unsafe.set output_div "innerHTML" (Js.string new_content);
  Js.Unsafe.set output_div "scrollTop" (Js.Unsafe.get output_div "scrollHeight")

let update_container (id : string) (content : string) : unit =
  let container = Dom_html.getElementById id in
  container##.innerHTML := Js.string content

let html_escape (s : string) : string =
  let s = String.concat "&amp;"
    (String.split_on_char '&' s) in
  let s = String.concat "&lt;" (String.split_on_char '<' s) in
  let s = String.concat "&gt;"
    (String.split_on_char '>' s) in
  s
