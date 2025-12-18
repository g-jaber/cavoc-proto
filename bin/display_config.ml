(* ========================================================
   DISPLAY_CONFIG: Evaluation state visualization
   ========================================================
   Renders the evaluation state (environment, store, config):
   - generate_store_html: Formats store contents for display
   - generate_ienv_html: Formats environment for display
   - display_conf: Main function that initializes ACE editors
     and displays current program state with syntax highlighting
   - Handles HTML generation and ACE editor configuration
*)

open Js_of_ocaml

let generate_store_html (store_str : string) : string =
  store_str |> String.split_on_char ';'
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.map (fun line -> Printf.sprintf "<div>%s</div>" line)
  |> String.concat "\n"

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
  | `List items ->
      let all_pairs =
        items
        |> List.map (function `Assoc pairs -> pairs | _ -> [])
        |> List.flatten
      in
      
      let filtered_pairs =
        List.filter (fun (k, v) -> not (is_duplicate k v)) all_pairs
      in

      if filtered_pairs = [] then
        "<div style='padding: 10px; color: #75715e; font-style: italic;'>Store is empty or all variables are hidden by ienv.</div>"
      else
        filtered_pairs
        |> List.map (fun (k, v) ->
            let v_str =
              match v with
              | `String s -> s
              | _ -> Yojson.Safe.to_string v
            in
            Printf.sprintf
              "<div class=\"stack-item\" style='display: flex; flex-direction: row; align-items: baseline; padding: 8px; margin-bottom: 5px;'>\n\
               <span style='color:#fd971f; font-weight: bold; min-width: 50px; margin-right: 10px;'>%s</span>\n\
               <span style='color:#e6db74; font-family: monospace; white-space: pre-wrap; word-break: break-all;'>%s</span>\n\
               </div>"
              (Ui_helpers.html_escape k) (Ui_helpers.html_escape v_str))
        |> String.concat "\n"
        |> Printf.sprintf "<div style='padding: 10px; display: flex; flex-direction: column;'>%s</div>"

  | _ -> "<div style='padding: 10px; color: #f92672;'>Invalid store format (expected List of Objects)</div>"

let normalize_ienv = function
  | `Assoc _ as a -> a
  | `List [`Assoc fields; `List exprs] ->
      let extra =
        exprs
        |> List.mapi (fun i v ->
            ("stack" ^ string_of_int i, v))
      in
      `Assoc (fields @ extra)
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
              "<div id='ienv-item-%s' class=\"stack-item\" style='position: relative; height: auto;'>\n\
               <span class='ienv-id' style='color:#75715e; margin-right: 10px;'>%s</span>\n\
               <pre id='ienv-content-%s-%i' class='ienv-code'>%s</pre>\n\
            \ </div>"
              id id id i (Ui_helpers.html_escape v_str))
        |> String.concat "\n" in
      Printf.sprintf
        "<div style='padding: 20px; height: 100%%; overflow-y: auto; \
         background:#272822; font-family:monospace;'>\n\
         <h3 style='color: #a6e22e; margin-top: 0; margin-bottom: 20px;'>\n\
        \    Interactive Environment\n\
        \  </h3>\n\
        \  <div id='ienv-list' style='display: flex; flex-direction: column; gap: 8px;'>\n\
        \    %s\n\
        \  </div>\n\
        \ </div>"
        items_html
  | _ ->
      Printf.sprintf
        "<div style='padding: 20px; color: #e74c3c; background:#272822; font-family:monospace;'>\n\
        \  <h3 style='color: #e74c3c; margin-top: 0;'>✗ Format invalide</h3>\n\
        \  <p>Expected a JSON object (Assoc), but received something else.</p>\n\
        \  <div style='background: #2d2e27; padding: 15px; border-radius: 5px; \
         margin-top: 15px; overflow-x: auto; color:#f8f8f2;'>\n\
        \    <code>%s</code>\n\
        \  </div>\n\
        \ </div>"
        (Yojson.Safe.pretty_to_string ienv_obj)

let display_conf conf_json : unit =
  let conf_str = Yojson.Safe.pretty_to_string conf_json in
  let config_editor = Js.Unsafe.get Js.Unsafe.global "configEditor_instance" in
  let session = Js.Unsafe.get config_editor "session" in
  Js.Unsafe.meth_call session
    "setValue"
    [| Js.Unsafe.inject (Js.string conf_str) |]
  |> ignore;
  Js.Unsafe.meth_call config_editor "clearSelection" [||] |> ignore;

  match conf_json with
  | `Assoc fields -> (
      let ienv_opt = List.assoc_opt "ienv" fields in

      (match List.assoc_opt "store" fields with
      | Some (`List _ as store_json) ->
          let store_html = generate_store_html_from_json store_json ienv_opt in
          Ui_helpers.update_container "store" store_html
      | Some (`String store_str) ->
          let store_html = generate_store_html store_str in
          Ui_helpers.update_container "store" store_html
      | _ -> Ui_helpers.update_container "store" "<div>No store data available</div>");

      match ienv_opt with
      | Some ienv_obj ->
          let ienv_html = generate_ienv_html ienv_obj in
          Ui_helpers.update_container "ienv" ienv_html;
          
          let dom_list_to_list node_list =
            let rec loop i acc =
              if i < 0 then acc
              else
                match Js.Opt.to_option (node_list##item (i)) with
                | Some node -> loop (i - 1) (node :: acc)
                | None -> acc
            in
            loop (node_list##.length - 1) []
          in
          
          let ace = Js.Unsafe.get Js.Unsafe.global "ace" in
          let code_blocks = Dom_html.document##querySelectorAll (Js.string "#ienv .ienv-code") in
          let code_blocks_list = dom_list_to_list code_blocks in
  
          List.iter (fun node ->
            let element_opt = Dom_html.CoerceTo.element node in
            let element = Js.Opt.get element_opt (fun () -> failwith "Not an element") in

            let id = Js.to_string element##.id in
            let text_opt = element##.textContent in
            let text = Js.to_string (Js.Opt.get text_opt (fun () -> Js.string "")) in
            
            element##.innerHTML := Js.string "";

            let editor = Js.Unsafe.meth_call ace "edit" [|
                Js.Unsafe.inject (Js.string id) |] in
            let session = Js.Unsafe.get editor "session" in
            let renderer = Js.Unsafe.get editor "renderer" in

            Js.Unsafe.meth_call editor "setTheme" [|
                Js.Unsafe.inject (Js.string "ace/theme/monokai") |] |> ignore;
            Js.Unsafe.meth_call renderer "setShowGutter" [|
                Js.Unsafe.inject Js._false |] |> ignore;
                
            Js.Unsafe.meth_call editor "setReadOnly" [| Js.Unsafe.inject Js._true |] |> ignore;
            Js.Unsafe.meth_call editor "setShowPrintMargin" [|
                Js.Unsafe.inject Js._false |] |> ignore;
            Js.Unsafe.meth_call editor "setHighlightActiveLine" [| Js.Unsafe.inject Js._false |] |> ignore;
            Js.Unsafe.meth_call editor "setHighlightGutterLine" [|
                Js.Unsafe.inject Js._false |] |> ignore;
            Js.Unsafe.meth_call editor "setOptions" [| Js.Unsafe.inject (Js.Unsafe.obj [| ("maxLines", Js.Unsafe.inject (Js.Unsafe.meth_call session "getScreenLength" [||])) |]) |] |> ignore;
            
            Js.Unsafe.meth_call session "setMode" [|
                Js.Unsafe.inject (Js.string "ace/mode/ocaml") |] |> ignore;
            Js.Unsafe.meth_call session "setValue" [| Js.Unsafe.inject (Js.string text) |] |> ignore;
            Js.Unsafe.meth_call editor "clearSelection" [||] |> ignore;
            
            let new_height = Js.Unsafe.meth_call session "getScreenLength" [||] |> Js.Unsafe.coerce |> Js.to_float in
            let style = element##.style in
            style##.height := Js.string (Printf.sprintf "%fpx" (new_height *. 16.0 +. 4.0));
            Js.Unsafe.meth_call editor "resize" [| |] |> ignore;

            let container = Js.Unsafe.get editor "container" in
            let container_style = Js.Unsafe.get container "style" in
            Js.Unsafe.set container_style "border" (Js.string "none");
            Js.Unsafe.set container_style "margin" (Js.string "0");
            Js.Unsafe.set container_style "padding" (Js.string "0");
            Js.Unsafe.set container_style "backgroundColor" (Js.string "transparent");

          ) code_blocks_list;

      | None -> Ui_helpers.update_container "ienv" "<div>No ienv data available</div>")
  | _ ->
      Ui_helpers.print_to_output "Invalid JSON format"
