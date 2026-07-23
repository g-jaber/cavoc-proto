(* ========================================================
   DISPLAY_CONFIG: Evaluation state visualization
   ========================================================
   Renders the evaluation state (environment, store, config):
   - generate_store_html_from_json: Formats store contents for display
   - generate_ienv_html: Formats environment for display
   - display_conf: Main function that initializes ACE editors
     and displays current program state with syntax highlighting
   - Handles HTML generation and ACE editor configuration
*)

open Js_of_ocaml

let generate_store_html_from_json (store_json : Yojson.Safe.t) (ienv_json : Yojson.Safe.t option) : string =
  let ienv_pairs =
    match ienv_json with
    | Some (`Assoc fields) -> fields
    | _ -> []
  in

  let is_duplicate k v =
    match List.assoc_opt k ienv_pairs with
    | Some v_ienv -> v = v_ienv
    | None -> false
  in

  match store_json with
  | `Assoc items ->
      let all_pairs =
        List.map snd items
        |> List.map (function `Assoc pairs -> pairs | _ -> [])
        |> List.flatten
      in
      
      let filtered_pairs =
        List.filter (fun (k, v) -> not (is_duplicate k v)) all_pairs
      in

      if filtered_pairs = [] then
        "<div class=\"store-empty\">The store is empty, or all its variables \
         are hidden by the IEnv.</div>"
      else
        filtered_pairs
        |> List.map (fun (k, v) ->
            let v_str =
              match v with
              | `String s -> s
              | _ -> Yojson.Safe.to_string v
            in
            Printf.sprintf
              "<div class=\"stack-item store-row\">\n\
               <span class=\"store-key\">%s</span>\n\
               <span class=\"store-val\">%s</span>\n\
               </div>"
              (Ui_helpers.html_escape k) (Ui_helpers.html_escape v_str))
        |> String.concat "\n"
        |> Printf.sprintf "<div class=\"store-list\">%s</div>"

  | _ ->
      "<div class=\"store-invalid\">Invalid store format (expected List of \
       Objects)</div>"

let normalize_ienv = function
  (* CPS: continuations are values (named). *)
  | `Assoc _ as a -> a
  (* Direct style: values are named (fields)
                   execution contexts (stack = exprs) are unamed *)
  | `List [`Assoc fields; `List exprs] ->
      begin
        match exprs with
        | [] -> `Assoc fields (* no execution context *)
        | active :: rest ->
            let active = ("active-ctx", active) in
            (* Reverse stack order to keep numbers stable *)
            let ctx = List.rev rest in
            let ctx = List.mapi (fun i v -> "stack" ^ string_of_int i, v) ctx in
            `Assoc (active :: fields @ ctx)
      end
  (* An unexpected shape is returned unchanged: generate_ienv_html then falls
     through to its error panel rather than crashing the interaction. *)
  | other -> other

let generate_ienv_html (ienv_obj : Yojson.Safe.t) : string =
  match normalize_ienv ienv_obj with
  | `Assoc fields ->
      let items_html =
        fields
        |> List.mapi (fun i (id, v) ->
            let v_str =
              match v with
              | `String s -> s
              | _ -> Yojson.Safe.to_string v in
            Printf.sprintf
              "<div id='ienv-item-%s' class=\"stack-item ienv-item\">\n\
               <span class='ienv-id'>%s</span>\n\
               <pre id='ienv-content-%s-%i' class='ienv-code'>%s</pre>\n\
            \ </div>"
              id id id i (Ui_helpers.html_escape v_str))
        |> String.concat "\n" in
      Printf.sprintf
        "<div class=\"ienv-panel\">\n\
        \  <h3 class=\"ienv-title\">Interactive Environment</h3>\n\
        \  <div id='ienv-list'>\n\
        \    %s\n\
        \  </div>\n\
        \ </div>"
        items_html
  | _ ->
      Printf.sprintf
        "<div class=\"ienv-panel ienv-panel-error\">\n\
        \  <h3 class=\"ienv-title\">✗ Invalid format</h3>\n\
        \  <p>Expected a JSON object (Assoc), but received something else.</p>\n\
        \  <div class=\"ienv-error-box\">\n\
        \    <code>%s</code>\n\
        \  </div>\n\
        \ </div>"
        (Yojson.Safe.pretty_to_string ienv_obj)

(* The Configuration tab is an ACE editor, reached through the instance the
   page stored on the window object (see cavocInitAce in front/common.js). *)
let set_config_editor_text (text : string) : unit =
  let config_editor = Js.Unsafe.get Js.Unsafe.global "configEditor" in
  let session = Js.Unsafe.get config_editor "session" in
  Js.Unsafe.meth_call session "setValue" [| Js.Unsafe.inject (Js.string text) |]
  |> ignore;
  Js.Unsafe.meth_call config_editor "clearSelection" [||] |> ignore

(* The ACE editors created for the entries of the IEnv panel. A fresh set is
   built on every configuration, so the previous ones are destroyed first:
   otherwise each still holds its listeners and detached DOM, and they pile up
   over a long interaction. *)
let ienv_editors : Js.Unsafe.any list ref = ref []

let destroy_ienv_editors () =
  List.iter (fun ed -> ignore (Js.Unsafe.meth_call ed "destroy" [||])) !ienv_editors;
  ienv_editors := []

(* Untyped-JS boundary: ACE has no js_of_ocaml bindings. Registers the editor
   so destroy_ienv_editors can release it on the next render. *)
let mount_ienv_code_editor (element : Dom_html.element Js.t) : unit =
  let ace = Js.Unsafe.get Js.Unsafe.global "ace" in
  let s = Js.string in
  let call obj meth args = ignore (Js.Unsafe.meth_call obj meth args) in
  let id = Js.to_string element##.id in
  let text = Js.to_string (Js.Opt.get element##.textContent (fun () -> s "")) in
  element##.innerHTML := s "";
  let editor : Js.Unsafe.any =
    Js.Unsafe.meth_call ace "edit" [| Js.Unsafe.inject (s id) |] in
  ienv_editors := editor :: !ienv_editors;
  let session = Js.Unsafe.get editor "session" in
  let renderer = Js.Unsafe.get editor "renderer" in
  call editor "setTheme" [| Js.Unsafe.inject (s "ace/theme/monokai") |];
  call renderer "setShowGutter" [| Js.Unsafe.inject Js._false |];
  call editor "setReadOnly" [| Js.Unsafe.inject Js._true |];
  call editor "setShowPrintMargin" [| Js.Unsafe.inject Js._false |];
  call editor "setHighlightActiveLine" [| Js.Unsafe.inject Js._false |];
  call editor "setHighlightGutterLine" [| Js.Unsafe.inject Js._false |];
  call editor "setOptions"
    [| Js.Unsafe.inject
         (Js.Unsafe.obj
            [| ("maxLines",
                Js.Unsafe.inject (Js.Unsafe.meth_call session "getScreenLength" [||])) |]) |];
  call session "setMode" [| Js.Unsafe.inject (s "ace/mode/ocaml") |];
  call session "setValue" [| Js.Unsafe.inject (s text) |];
  call editor "clearSelection" [||];
  let lines =
    Js.Unsafe.meth_call session "getScreenLength" [||]
    |> Js.Unsafe.coerce |> Js.to_float in
  element##.style##.height := s (Printf.sprintf "%fpx" ((lines *. 16.0) +. 4.0));
  call editor "resize" [||];
  let container_style = Js.Unsafe.get (Js.Unsafe.get editor "container") "style" in
  Js.Unsafe.set container_style "border" (s "none");
  Js.Unsafe.set container_style "margin" (s "0");
  Js.Unsafe.set container_style "padding" (s "0");
  Js.Unsafe.set container_style "backgroundColor" (s "transparent")

(* Shown in place of a configuration whose branch has no continuation, i.e. one
   where Proponent stopped playing or the program diverges. *)
let display_terminal_conf (reason : string) : unit =
  destroy_ienv_editors ();
  set_config_editor_text reason;
  let notice =
    Printf.sprintf "<div class=\"conf-notice\">%s</div>"
      (Ui_helpers.html_escape reason) in
  Ui_helpers.update_container "store" notice;
  Ui_helpers.update_container "ienv" notice

let display_conf conf_json : unit =
  destroy_ienv_editors ();
  set_config_editor_text (Yojson.Safe.pretty_to_string conf_json);

  match conf_json with
  | `Assoc fields -> (
      let ienv_opt = List.assoc_opt "ienv" fields in

      (match List.assoc_opt "store" fields with
      | Some (`Assoc _ as store_json) ->
          let store_html = generate_store_html_from_json store_json ienv_opt in
          Ui_helpers.update_container "store" store_html
      | _ ->
          Ui_helpers.update_container "store"
            "<div class=\"panel-empty\">No store data available.</div>");

      match ienv_opt with
      | Some ienv_obj ->
          Ui_helpers.update_container "ienv" (generate_ienv_html ienv_obj);
          Dom_html.document##querySelectorAll (Js.string "#ienv .ienv-code")
          |> Dom.list_of_nodeList
          |> List.iter (fun node ->
                 Js.Opt.iter (Dom_html.CoerceTo.element node)
                   mount_ienv_code_editor);

      | None -> Ui_helpers.update_container "ienv" "<div>No ienv data available</div>")
  | _ ->
      Ui_helpers.print_to_output "Invalid JSON format"
