(* =====================================
   HELP_MODAL: Help system management
   =====================================
   Handles the help modal and documentation:
   - parse_markdown: Converts markdown to HTML using marked.js
   - show_help: Opens the help modal and loads help.md
   - close_help: Closes the help modal window
   - init_help_events: Sets up event handlers for help buttons
*)

open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix

let parse_markdown (md : string) : string =
  let marked = Js.Unsafe.get Js.Unsafe.global "marked" in
  Js.to_string (Js.Unsafe.meth_call marked "parse" [| Js.Unsafe.inject (Js.string md) |])

let show_help () =
  let modal = Dom_html.getElementById "help-modal" in
  let content_div = Dom_html.getElementById "markdown-content" in
  
  modal##.style##.display := Js.string "block";
  
  let%lwt response = Js_of_ocaml_lwt.XmlHttpRequest.get "help.md" in
  let html_content = 
    if response.code = 200 then
      parse_markdown response.content
    else
      "<p style='color:red'>Error loading help file.</p>"
  in
  
  content_div##.innerHTML := Js.string html_content;
  Lwt.return ()

let close_help () =
  let modal = Dom_html.getElementById "help-modal" in
  modal##.style##.display := Js.string "none";
  Js._true

let init_help_events () =
  let help_btn = Dom_html.getElementById "help-btn" in
  let close_span =
    let elements = Dom_html.document##getElementsByClassName (Js.string "close-btn") in
    Js.Opt.get (elements##item 0) (fun () -> failwith "close-btn not found")
  in
  let modal = Dom_html.getElementById "help-modal" in

  help_btn##.onclick := Dom_html.handler (fun _ ->
    Lwt.ignore_result (show_help ());
    Js._true
  );

  Js.Opt.iter (Dom_html.CoerceTo.element close_span) (fun el ->
    el##.onclick := Dom_html.handler (fun _ -> close_help ())
  );

  Dom_html.window##.onclick := Dom_html.handler (fun e ->
    Js.Opt.case e##.target
      (fun () -> Js._true)
      (fun target ->
        if (target :> Dom_html.eventTarget Js.t) = (modal :> Dom_html.eventTarget Js.t) then
          close_help ()
        else
          Js._true
      )
  )
