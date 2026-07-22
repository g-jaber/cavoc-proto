(* ========================================
   UI_HELPERS: Low-level HTML/DOM utilities
   ========================================
   Provides basic functions for DOM manipulation and output handling:
   - set_button_enabled: Toggles a button between its enabled and disabled
     presentation, via the "btn-disabled" class rather than inline styles
   - print_to_output: Appends formatted text to the console div
   - update_container: Updates HTML content of a specific element
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

let update_container (id : string) (content : string) : unit =
  let container = Dom_html.getElementById id in
  container##.innerHTML := Js.string content

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
