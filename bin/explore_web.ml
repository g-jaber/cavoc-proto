open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix

(* ---- Ace editor pour l'onglet ienv ---- *)

(* Fetches the content from the HTML editor fields *)
let editor_content = ref ""
let signature_content = ref ""

let fetch_editor_content () =
  let editor = Js.Unsafe.get Js.Unsafe.global "editor_instance" in
  let signature_editor =
    Js.Unsafe.get Js.Unsafe.global "signatureEditor_instance" in

  (* Fetch content from the ACE editor (this preserves newlines as \n) *)
  editor_content := Js.to_string (Js.Unsafe.meth_call editor "getValue" [||]);
  signature_content :=
    Js.to_string (Js.Unsafe.meth_call signature_editor "getValue" [||])

(* Redirects OCaml print functions to output in the HTML div with id "output" *)
let print_to_output str =
  let output_div = Dom_html.getElementById "console" in
  let current_content = Js.to_string (Js.Unsafe.get output_div "innerHTML") in
  let new_content = current_content ^ "<pre>" ^ str ^ "</pre>" in
  Js.Unsafe.set output_div "innerHTML" (Js.string new_content);
  Js.Unsafe.set output_div "scrollTop" (Js.Unsafe.get output_div "scrollHeight")

(* Mutable list to store previous moves *)
let previous_moves : string list ref = ref []

(*Shows the moves in the html*)
let display_previous_moves () : unit =
  let moves_string = String.concat " ; " !previous_moves in
  let move_display = Dom_html.getElementById "history" in
  Js.Unsafe.set move_display "textContent" (Js.string moves_string)

(* Adds a move to the previous moves list and updates the DOM *)
let add_move move =
  previous_moves := !previous_moves @ [ move ];
  display_previous_moves ()

(* Clears the previous moves list and the DOM display *)
let flush_moves () =
  previous_moves := [];
  display_previous_moves ()

(* Updates the HTML content of a specific container by ID *)
let update_container (id : string) (content : string) : unit =
  let container = Dom_html.getElementById id in
  container##.innerHTML := Js.string content

(* Generates HTML for the "store" tab *)
let generate_store_html (store_str : string) : string =
  store_str |> String.split_on_char ';' |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.map (fun line -> Printf.sprintf "<div>%s</div>" line)
  |> String.concat "\n"

let extract_ienv_strings (ienv_json : Yojson.Safe.t) : string list =
  let of_assoc bindings =
    bindings
    |> List.filter_map (function
         | (name, `String expr) when expr <> "" ->
             Some (Printf.sprintf "let %s = %s" name expr)
         | _ -> None) in
  let rec aux j =
    match j with
    | `Assoc bindings -> of_assoc bindings
    | `String s when s <> "" -> [ s ]
    | `List items -> items |> List.concat_map aux
    | _ -> [] in
  aux ienv_json

let html_escape (s : string) : string =
  let s = String.concat "&amp;" (String.split_on_char '&' s) in
  let s = String.concat "&lt;" (String.split_on_char '<' s) in
  let s = String.concat "&gt;" (String.split_on_char '>' s) in
  s

let replace_all (s : string) ~(sub : string) ~(by : string) : string =
  let sub_len = String.length sub in
  if sub_len = 0 then s
  else
    let rec aux i acc =
      if i > String.length s - sub_len then
        let rest = String.sub s i (String.length s - i) in
        String.concat "" (List.rev (rest :: acc))
      else if String.sub s i sub_len = sub then aux (i + sub_len) (by :: acc)
      else aux (i + 1) (String.sub s i 1 :: acc) in
    aux 0 []

let highlight_ocaml (s : string) : string =
  (* on commence par échapper le HTML *)
  let s = html_escape s in
  (* palette monokai approximative *)
  let keyword_color =
    "#f92672"
    (* rose vif, comme Ace monokai *) in
  let keywords =
    [
      "let";
      "fun";
      "match";
      "with";
      "if";
      "then";
      "else";
      "assert";
      "in";
      "rec";
      "type";
      "module";
      "open";
    ] in
  List.fold_left
    (fun acc kw ->
      let by =
        Printf.sprintf "<span style='color:%s;font-weight:bold;'>%s</span>"
          keyword_color kw in
      replace_all acc ~sub:kw ~by)
    s keywords

let generate_ienv_html (ienv_obj : Yojson.Safe.t) : string =
  match ienv_obj with
  | `List [] ->
      "<div style='padding: 20px; color: #999; text-align: center;\n\
      \                     background:#272822; font-family:monospace;'>\n\
      \           <h3 style='color: #999;'>Interactive Environment</h3>\n\
      \           <p>Empty environment</p>\n\
      \         </div>"
  | `List items ->
      let items_html =
        items
        |> List.mapi (fun i item ->
               match item with
               | `String s ->
                   (* String directe : on colore comme du code OCaml *)
                   let displayed = highlight_ocaml s in
                   Printf.sprintf
                     "<div class=\"stack-item\" style='color:#f8f8f2;'>\n\
                     \                          <span style='color: #75715e; \
                      margin-right: 10px;'>[%d]</span>\n\
                     \                          <pre style='display:inline; \
                      margin:0;'>%s</pre>\n\
                     \                        </div>"
                     i displayed
               | `Assoc fields ->
                   (* Assoc : on colorise les valeurs string comme du code *)
                   let fields_str =
                     fields
                     |> List.map (fun (k, v) ->
                            let v_str =
                              match v with
                              | `String s -> highlight_ocaml s
                              | _ -> html_escape (Yojson.Safe.to_string v) in
                            Printf.sprintf "<strong>%s:</strong> %s" k v_str)
                     |> String.concat ", " in
                   Printf.sprintf
                     "<div class=\"stack-item\" style='color:#f8f8f2;'>\n\
                     \                          <span style='color: #75715e; \
                      margin-right: 10px;'>[%d]</span>\n\
                     \                          <pre style='display:inline; \
                      margin:0;'>%s</pre>\n\
                     \                        </div>"
                     i fields_str
               | `List sub_items ->
                   (* Sous-liste : on essaie d’en extraire des lignes de code *)
                   let lines = extract_ienv_strings (`List sub_items) in
                   let displayed =
                     match lines with
                     | [] ->
                         (* fallback : JSON brut colorisé *)
                         let sub_str = Yojson.Safe.to_string (`List sub_items) in
                         highlight_ocaml sub_str
                     | _ ->
                         lines |> List.map highlight_ocaml
                         |> String.concat "<br/>" in
                   Printf.sprintf
                     "<div class=\"stack-item\" style='color:#f8f8f2;'>\n\
                     \                          <span style='color: #75715e; \
                      margin-right: 10px;'>[%d]</span>\n\
                     \                          <pre style='display:inline; \
                      margin:0;'>%s</pre>\n\
                     \                        </div>"
                     i displayed
               | _ ->
                   (* Autre cas : on affiche le JSON brut, échappé, sans coloration spécifique *)
                   let s = html_escape (Yojson.Safe.to_string item) in
                   Printf.sprintf
                     "<div class=\"stack-item\" style='color:#f8f8f2;'>\n\
                     \                          <span style='color: #75715e; \
                      margin-right: 10px;'>[%d]</span>\n\
                     \                          <pre style='display:inline; \
                      margin:0;'>%s</pre>\n\
                     \                        </div>"
                     i s)
        |> String.concat "\n" in

      Printf.sprintf
        "<div style='padding: 20px; height: 100%%; overflow-y: auto;\n\
        \                      background:#272822; font-family:monospace;'>\n\
        \             <h3 style='color: #a6e22e; margin-top: 0; margin-bottom: \
         20px;'>\n\
        \               Interactive Environment\n\
        \               <span style='color: #75715e; font-size: 0.8em; \
         margin-left: 10px;'>(%d items)</span>\n\
        \             </h3>\n\
        \             <div style='display: flex; flex-direction: column; gap: \
         8px;'>\n\
        \               %s\n\
        \             </div>\n\
        \           </div>"
        (List.length items) items_html
  | _ ->
      Printf.sprintf
        "<div style='padding: 20px; color: #e74c3c;\n\
        \                     background:#272822; font-family:monospace;'>\n\
        \           <h3 style='color: #e74c3c; margin-top: 0;'>✗ Format \
         invalide</h3>\n\
        \           <p>Expected a JSON array, but received something else.</p>\n\
        \           <div style='background: #2d2e27; padding: 15px; \
         border-radius: 5px; \n\
        \                       margin-top: 15px; overflow-x: auto; \
         color:#f8f8f2;'>\n\
        \             <code>%s</code>\n\
        \           </div>\n\
        \         </div>"
        (Yojson.Safe.pretty_to_string ienv_obj)

(* Main display configuration function *)
let display_conf conf_json : unit =
  (* 1) Affiche le JSON brut dans l’éditeur config (comme avant) *)
  let conf_str = Yojson.Safe.pretty_to_string conf_json in
  let config_editor = Js.Unsafe.get Js.Unsafe.global "configEditor_instance" in
  let session = Js.Unsafe.get config_editor "session" in
  Js.Unsafe.meth_call session "setValue"
    [| Js.Unsafe.inject (Js.string conf_str) |]
  |> ignore;
  Js.Unsafe.meth_call config_editor "clearSelection" [||] |> ignore;

  match conf_json with
  | `Assoc fields -> (
      (* store : inchangé *)
      (match List.assoc_opt "store" fields with
      | Some (`String store_str) ->
          let store_html = generate_store_html store_str in
          update_container "store" store_html
      | _ -> update_container "store" "<div>No store data available</div>");

      (* ienv : HTML + Ace coloré *)
      match List.assoc_opt "ienv" fields with
      | Some ienv_obj ->
          let ienv_html = generate_ienv_html ienv_obj in
          update_container "ienv" ienv_html
      | None -> update_container "ienv" "<div>No ienv data available</div>")
  | _ -> print_to_output "Invalid JSON format"

(*function which generate clickable component on the DOM*)
let generate_clickables moves =
  (*let moves = moves @ [ (-1, "Stop") ] in*)
  let moves_list = Dom_html.getElementById "moves-list" in
  moves_list##.innerHTML := Js.string "";

  (* Clear existing elements *)
  List.iteri
    (fun index (id, move) ->
      let checkbox_div = Dom_html.createDiv Dom_html.document in
      let checked_attr = if index = 0 then " checked" else "" in
      checkbox_div##.innerHTML :=
        Js.string
          (Printf.sprintf
             "<input type='radio' name='move' id='move_%d'%s> <label \
              for='move_%d'>%s</label>"
             id checked_attr id move);
      Dom.appendChild moves_list checkbox_div)
    moves

let clear_list () : unit =
  let moves_list = Dom_html.getElementById "moves-list" in
  moves_list##.innerHTML := Js.string ""

let get_chosen_move _ =
  let select_btn_opt = Dom_html.getElementById_opt "select-btn" in
  let load_btn_opt = Dom_html.getElementById_opt "load-btn" in
  let stop_btn_opt = Dom_html.getElementById_opt "stop-btn" in
  match (select_btn_opt, load_btn_opt, stop_btn_opt) with
  | (None, _, _) -> Lwt.return (-2) (* No select button found *)
  | (_, None, _) -> Lwt.return (-2) (* No load button found *)
  | (_, _, None) -> Lwt.return (-2) (* No load button found *)
  | (Some select_btn, Some load_btn, Some stop_btn) ->
      Lwt.choose
        [
          ( Lwt_js_events.click select_btn >>= fun _ ->
            let moves_list_opt = Dom_html.getElementById_opt "moves-list" in
            match moves_list_opt with
            | None -> Lwt.return (-2) (* No moves list found *)
            | Some moves_list ->
                let children = Dom.list_of_nodeList moves_list##.childNodes in
                let selected_move =
                  List.fold_left
                    (fun acc child ->
                      match
                        Js.Opt.to_option (Dom_html.CoerceTo.element child)
                      with
                      | None -> acc
                      | Some element -> (
                          match
                            element##querySelector
                              (Js.string "input[type='radio']")
                          with
                          | exception _ -> acc
                          | input_opt -> (
                              match Js.Opt.to_option input_opt with
                              | None -> acc
                              | Some input -> (
                                  let input = Dom_html.CoerceTo.input input in
                                  match Js.Opt.to_option input with
                                  | None -> acc
                                  | Some radio_input ->
                                      if Js.to_bool radio_input##.checked then
                                        let id_str =
                                          Js.to_string radio_input##.id in
                                        match
                                          String.split_on_char '_' id_str
                                        with
                                        | [ _; num_str ] ->
                                            int_of_string num_str
                                        | _ -> acc
                                      else acc))))
                    (-4) children in
                Lwt.return selected_move );
          (Lwt_js_events.click load_btn >>= fun _ -> Lwt.return (-1));
          (Lwt_js_events.click stop_btn >>= fun _ -> Lwt.return (-1));
        ]

(* Overrides default print functions to redirect to the HTML output div *)
let () =
  Printexc.record_backtrace true;
  Sys_js.set_channel_flusher stdout print_to_output;
  Sys_js.set_channel_flusher stderr print_to_output

(* Builds and evaluates the OGS LTS based on the provided code content *)
let evaluate_code () =
  flush_moves ();
  fetch_editor_content ();
  let module OpLang = Refml.RefML.WithAVal (Util.Monad.ListB) in
  let module CpsLang = Lang.Cps.MakeComp (OpLang) () in
  let module IntLang = Lang.Interactive.Make (CpsLang) in
  let module TypingLTS = Ogs.Typing.Make (IntLang) in
  let module OGS_LTS = Ogs.Ogslts.Make (IntLang) (TypingLTS) in
  let lexBuffer_code = Lexing.from_string !editor_content in
  let lexBuffer_sig = Lexing.from_string !signature_content in
  let (interactive_env, store, name_ctxP, name_ctxO) =
    IntLang.get_typed_ienv lexBuffer_code lexBuffer_sig in
  let init_conf =
    OGS_LTS.Passive
      (OGS_LTS.init_pconf store interactive_env name_ctxP name_ctxO) in
  let module IBuild = Lts.Interactive_build.Make (OGS_LTS) in
  let show_move move = add_move move in
  let show_conf conf : unit = display_conf conf in

  let show_moves_list (json_list : Yojson.Safe.t list) =
    (*let id_of i (v : Yojson.Safe.t) =
      match v with
      | `Assoc fields -> (
          match List.assoc_opt "id" fields with Some (`Int n) -> n | _ -> i)
      | _ -> i in *)
    let moves =
      List.mapi (fun i v -> (i, Yojson.Safe.pretty_to_string v)) json_list in
    generate_clickables moves in
  let get_move n =
    let n = n + 1 in
    let%lwt i = get_chosen_move n in
    print_to_output ("Chosen move index: " ^ string_of_int i);
    match i with
    | i when i >= 0 && i < n -> Lwt.return i
    | -1 ->
        clear_list ();
        Lwt.fail (Failure "Stop")
    | -2 -> Lwt.fail (Failure "No button")
    | _ ->
        print_to_output "error : unknown";
        Lwt.fail (Failure "Unknown error") in
  IBuild.interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
    init_conf

(* Do page init, creating the callback on the submit button, and managing some button looks*)
let rec init_page () =
  Printexc.record_backtrace true;
  let button = Dom_html.getElementById "submit" in
  let select_button = Dom_html.getElementById "select-btn" in
  let stop_button = Dom_html.getElementById "stop-btn" in

  (* Disable the Select button by default *)
  Js.Unsafe.set select_button "disabled" Js._true;
  Js.Unsafe.set select_button "style"
    (Js.string "background-color: grey; cursor: not-allowed;");
  Js.Unsafe.set select_button "title"
    (Js.string "You must be evaluating code to select an move");

  (* Disable the Stop button by default *)
  Js.Unsafe.set stop_button "disabled" Js._true;
  Js.Unsafe.set stop_button "style"
    (Js.string "background-color: grey; cursor: not-allowed;");
  Js.Unsafe.set stop_button "title"
    (Js.string "You must be evaluating code to select an move");

  (* Restore Evaluate button's original state *)
  Js.Unsafe.set button "disabled" Js._false;
  Js.Unsafe.set button "style"
    (Js.string "background-color: ''; cursor: pointer;");

  (* Set tooltip for the Evaluate button when it's disabled *)
  Js.Unsafe.set button "title"
    (Js.string "Stop evaluation to evaluate new code");

  Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
      let%lwt _ = Js_of_ocaml_lwt.Lwt_js_events.click button in
      (* Grey out the button after it is clicked *)
      Js.Unsafe.set button "disabled" Js._true;
      Js.Unsafe.set button "style"
        (Js.string "background-color: grey; cursor: not-allowed;");
      Js.Unsafe.set button "title"
        (Js.string "Stop evaluation to evaluate new code");

      (* Enable the Select button after evaluate is pressed *)
      Js.Unsafe.set select_button "disabled" Js._false;
      Js.Unsafe.set select_button "style"
        (Js.string "background-color: ''; cursor: pointer;");
      Js.Unsafe.set select_button "title"
        (Js.string "You must be evaluating code to select an move");

      (* Enable the Stop button after evaluate is pressed *)
      Js.Unsafe.set stop_button "disabled" Js._false;
      Js.Unsafe.set stop_button "style"
        (Js.string "background-color: ''; cursor: pointer;");
      Js.Unsafe.set stop_button "title"
        (Js.string "You must be evaluating code to select an move");

      Lwt.catch
        (fun () -> evaluate_code ())
        (function
          | Failure msg when msg = "Stop" ->
              (* Disable Select button again after Stop move *)
              Js.Unsafe.set select_button "disabled" Js._true;
              Js.Unsafe.set select_button "style"
                (Js.string "background-color: grey; cursor: not-allowed;");
              Js.Unsafe.set select_button "title"
                (Js.string "You must be evaluating code to select an move");

              (* Disable Stop button again after Stop move *)
              Js.Unsafe.set stop_button "disabled" Js._true;
              Js.Unsafe.set stop_button "style"
                (Js.string "background-color: grey; cursor: not-allowed;");
              Js.Unsafe.set stop_button "title"
                (Js.string "You must be evaluating code to select an move");

              (* Re-enable Evaluate button after stopping *)
              init_page ();
              (* Recursively call init_page to restore button *)
              Lwt.return_unit
          | exn ->
              (* init page again to allow for another exec *)
              init_page ();
              (* Handle other exceptions *)
              print_to_output ("Unhandled exception: " ^ Printexc.to_string exn);
              Lwt.return_unit))

(* "Main" entry point *)
let () = init_page ()
