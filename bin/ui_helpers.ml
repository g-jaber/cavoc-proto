(* ========================================
   UI_HELPERS: Low-level HTML/DOM utilities
   ========================================
   Provides basic functions for DOM manipulation and output handling:
   - set_button_enabled: Toggles a button between its enabled and disabled
     presentation, via the "btn-disabled" class rather than inline styles
   - set_element_locked: The same for a whole section of controls
   - print_to_output: Appends formatted text to the console div
   - html_escape: Escapes special characters for safe HTML display
*)

open Js_of_ocaml

(* The presentation of a disabled button is described by this class in
   front/style.css. *)
let disabled_class = Js.string "btn-disabled"

let default_disabled_title = "You must be evaluating code to do that"

let set_button_enabled ?(title = default_disabled_title) id state =
  match Dom_html.getElementById_opt id with
  | None -> ()
  | Some button ->
      if state then begin
        button##.classList##remove disabled_class;
        button##removeAttribute (Js.string "title");
        button##removeAttribute (Js.string "disabled")
      end
      else begin
        button##.classList##add disabled_class;
        button##setAttribute (Js.string "title") (Js.string title);
        button##setAttribute (Js.string "disabled") (Js.string "true")
      end

(* A section of controls the page has made inert, described by this class in
   front/option_window.css. Unlike a disabled button this leaves the element's
   own attributes alone, so it does not fight the locks front/scenario.js sets
   on the individual checkboxes. *)
let locked_class = Js.string "locked"

let set_element_locked id locked =
  match Dom_html.getElementById_opt id with
  | None -> ()
  | Some element ->
      if locked then element##.classList##add locked_class
      else element##.classList##remove locked_class

(* Switches the visible configuration tab to the one whose content div has id
   [name] (config / ienv / store / histo / console). Delegates to cavocShowTab
   in front/common.js, the single implementation the tab buttons also use, so
   the OCaml side can bring a tab forward - e.g. the Console when something is
   written there. *)
let show_tab (name : string) : unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.get Js.Unsafe.global "cavocShowTab")
    [| Js.Unsafe.inject (Js.string name) |]

(* Appends a new line to the console as a text node: [str] may carry program
   source, hence '<' and '&'. *)
let print_to_output str =
  match Dom_html.getElementById_opt "console" with
  | None -> ()
  | Some output_div ->
      let line = Dom_html.createPre Dom_html.document in
      line##.textContent := Js.some (Js.string str);
      Dom.appendChild output_div line;
      output_div##.scrollTop :=
        Js.number_of_float (float_of_int output_div##.scrollHeight)

(* Escapes the characters that are significant in HTML text and in quoted
   attribute values, so that the result can be interpolated in either. *)
let html_escape (s : string) : string =
  let buffer = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      Buffer.add_string buffer
        (match c with
        | '&' -> "&amp;"
        | '<' -> "&lt;"
        | '>' -> "&gt;"
        | '"' -> "&quot;"
        | '\'' -> "&#39;"
        | c -> String.make 1 c))
    s;
  Buffer.contents buffer
