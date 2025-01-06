open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix

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

(* print function to send output to the web console *)
let print_to_output str = ignore (Firebug.console##log str)

(* Mutable list to store previous actions *)
let previous_actions : string list ref = ref []

(*Shows the actions in the html*)
let display_previous_actions () : unit =
  let actions_string = String.concat " ; " !previous_actions in
  let action_display = Dom_html.getElementById "history" in
  Js.Unsafe.set action_display "textContent" (Js.string actions_string);
  print_to_output @@ "Actions played = " ^ actions_string

(* Adds an action to the previous actions list and updates the DOM *)
let add_action action =
  (* Debugging line *)
  previous_actions := !previous_actions @ [ action ];
  display_previous_actions ()

(* Clears the previous actions list and the DOM display *)
let flush_actions () =
  previous_actions := [];
  display_previous_actions ()

(* Updates the HTML content of a specific container by ID *)
let update_container (id : string) (content : string) : unit =
  let container = Dom_html.getElementById id in
  container##.innerHTML := Js.string content

(* Generates HTML for the "store" tab *)
let generate_store_html (store_str : string) : string =
  (* Split the store string into lines and wrap each in a <div> for display *)
  store_str |> String.split_on_char ';' |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.map (fun line -> Printf.sprintf "<div>%s</div>" line)
  |> String.concat "\n"

(* Generates HTML for the "ienv" tab *)
let generate_ienv_html (ienv_obj : Yojson.Safe.t) : string =
  match ienv_obj with
  | `Assoc fields -> (
      match
        (List.assoc_opt "ienv" fields, List.assoc_opt "ectx stack" fields)
      with
      | (Some (`Assoc ienv_content), Some (`List ectx_stack)) ->
          (* Generate ienv content as formatted text *)
          let ienv_html =
            ienv_content
            |> List.map (fun (key, value) ->
                   match value with
                   | `String v ->
                       Printf.sprintf "<div><strong>%s:</strong> %s</div>" key v
                   | _ ->
                       Printf.sprintf
                         "<div><strong>%s:</strong> (non-string value)</div>"
                         key)
            |> String.concat "\n" in

          (* Generate the stack content as a list of divs *)
          let stack_html =
            ectx_stack
            |> List.map (function
                 | `String s ->
                     Printf.sprintf "<div class=\"stack-item\">%s</div>" s
                 | _ -> "<div>(non-string value)</div>")
            |> String.concat "\n" in

          (* Wrap the ienv and stack in a flex container *)
          Printf.sprintf
            "<div style='display: flex; gap: 10px;'>\n\
            \              <div class='ienv-div' style='flex: 1; overflow-y: \
             auto; white-space: pre-wrap;'>%s</div>\n\
            \              <div id='stack-container' class='stack-div' \
             style='flex: 1; overflow-y: auto; height: 300px; overflow-y: \
             scroll;'>%s</div>\n\
            \            </div>"
            ienv_html stack_html
      | _ -> "<div>Invalid ienv structure</div>")
  | _ -> "<div>Invalid ienv format</div>"

(* Main display configuration function *)
let display_conf conf_json : unit =
  (* Pretty-print the JSON and display it in the ACE editor *)
  let conf_str = Yojson.Safe.pretty_to_string conf_json in
  let config_editor = Js.Unsafe.get Js.Unsafe.global "configEditor_instance" in
  let session = Js.Unsafe.get config_editor "session" in
  Js.Unsafe.meth_call session "setValue"
    [| Js.Unsafe.inject (Js.string conf_str) |]
  |> ignore;
  Js.Unsafe.meth_call config_editor "clearSelection" [||] |> ignore;

  (* Decode the JSON to fill store and ienv tabs *)
  match conf_json with
  | `Assoc fields -> (
      (* Handle "store" tab *)
      (match List.assoc_opt "store" fields with
      | Some (`String store_str) ->
          let store_html = generate_store_html store_str in
          update_container "store" store_html
      | _ -> update_container "store" "<div>No store data available</div>");

      (* Handle "ienv" tab *)
      match List.assoc_opt "ienv" fields with
      | Some ienv_obj ->
          let ienv_html = generate_ienv_html ienv_obj in
          update_container "ienv" ienv_html
      | _ -> update_container "ienv" "<div>No ienv data available</div>")
  | _ -> print_to_output "Invalid JSON format"

(*function wich generate clickable component on the DOM*)
let generate_clickables actions =
  let actions = actions @ [ (-1, "Stop") ] in
  let actions_list = Dom_html.getElementById "actions-list" in
  actions_list##.innerHTML := Js.string "";

  (* Clear existing elements *)
  List.iteri
    (fun index (id, action) ->
      let checkbox_div = Dom_html.createDiv Dom_html.document in
      let checked_attr =
        if index = 0 then " checked" else "" (* Check the first radio button *)
      in
      checkbox_div##.innerHTML :=
        Js.string
          (Printf.sprintf
             "<input type='radio' name='action' id='action_%d'%s> %s" id
             checked_attr action);
      Dom.appendChild actions_list checkbox_div)
    actions

let clear_list () : unit =
  let actions_list = Dom_html.getElementById "actions-list" in
  actions_list##.innerHTML := Js.string ""

let get_chosen_action _ =
  print_to_output "Waiting for a click.";
  let select_btn_opt = Dom_html.getElementById_opt "select-btn" in
  match select_btn_opt with
  | None -> Lwt.return (-2) (* No button found *)
  | Some btn -> (
      Lwt_js_events.click btn >>= fun _ ->
      let actions_list_opt = Dom_html.getElementById_opt "actions-list" in
      match actions_list_opt with
      | None -> Lwt.return (-2) (* No actions list found *)
      | Some actions_list ->
          let children = Dom.list_of_nodeList actions_list##.childNodes in
          print_to_output @@ string_of_int (List.length children);
          let selected_action =
            List.fold_left
              (fun acc child ->
                match Js.Opt.to_option (Dom_html.CoerceTo.element child) with
                | None -> acc
                | Some element -> (
                    (* Look for input[type='radio'] inside each div *)
                    match
                      element##querySelector (Js.string "input[type='radio']")
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
                                if Js.to_bool radio_input##.checked then (
                                  (* Log the action *)
                                  let id_str = Js.to_string radio_input##.id in
                                  print_to_output ("Selected ID: " ^ id_str);
                                  (* Extract the number from the id *)
                                  match String.split_on_char '_' id_str with
                                  | [ _; num_str ] -> int_of_string num_str
                                  | _ -> acc)
                                else acc))))
              (-4) children in
          Lwt.return selected_action)

(* Overrides default print functions to redirect to the HTML output div *)
let () =
  Printexc.record_backtrace true;
  Sys_js.set_channel_flusher stdout print_to_output;
  Sys_js.set_channel_flusher stderr print_to_output

(* Generates the LTS based on the selected mode and editor content *)

(* Builds and evaluates the OGS LTS based on the provided code content *)
let evaluate_code () =
  flush_actions ();
  (* Fetch editor content and store in refs *)
  fetch_editor_content ();
  (* Set options based on flags *)
  let module OpLang = Refml.RefML.WithAVal (Util.Monad.ListB) in
  (*  let module CpsLang = Lang.Cps.MakeComp (OpLang) in *)
  let module DirectLang = Lang.Direct.Make (OpLang) in
  (*  let module IntLang  = Lang.Interactive.Make (DirectLang : Lang.Interactive.LANG) in *)
  let module Int = Lts.Interactive.Make (DirectLang) in
  let module OGS_LTS = Ogs.Ogslts.Make (Int) in
  let lexBuffer_code = Lexing.from_string !editor_content in
  let lexBuffer_sig = Lexing.from_string !signature_content in
  let (interactive_env, store, name_ctxP, name_ctxO) =
    OGS_LTS.Int.IntLang.get_typed_ienv lexBuffer_code lexBuffer_sig in
  let init_conf =
    OGS_LTS.Passive
      (OGS_LTS.init_pconf store interactive_env name_ctxP name_ctxO) in
  let module IBuild = Lts.Interactive_build.Make (OGS_LTS) in
  let show_move action = add_action action in
  let show_conf conf : unit = display_conf conf in
  (*genere les cliquables et les ajoute dans la liste des coups possibles*)
  let show_moves_list results_list =
    (* Convert the moves list into a list of tuples with dummy ids for demonstration *)
    let actions =
      List.mapi (fun i results_list -> (i, results_list)) results_list in
    generate_clickables actions in
  (*event listener sur les cliquables renvoyant l'index de celui sur lequel l'utilisateur a cliqué *)
  let get_move n =
    let n = n + 1 in
    print_to_output "In get_move";
    let%lwt i = get_chosen_action n in
    print_to_output
      ("i = " ^ string_of_int i ^ " ; and number of actions =" ^ string_of_int n);
    match i with
    | i when i >= 0 && i < n ->
        print_to_output ("Selected action " ^ string_of_int i);
        Lwt.return i
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
  let button = Dom_html.getElementById "submit" in
  let select_button = Dom_html.getElementById "select-btn" in

  (* Disable the Select button by default *)
  Js.Unsafe.set select_button "disabled" Js._true;
  Js.Unsafe.set select_button "style"
    (Js.string "background-color: grey; cursor: not-allowed;");
  Js.Unsafe.set select_button "title"
    (Js.string "You must be evaluating code to select an action");

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
        (Js.string "You must be evaluating code to select an action");

      Lwt.catch
        (fun () -> evaluate_code ())
        (function
          | Failure msg when msg = "Stop" ->
              (* Disable Select button again after Stop action *)
              Js.Unsafe.set select_button "disabled" Js._true;
              Js.Unsafe.set select_button "style"
                (Js.string "background-color: grey; cursor: not-allowed;");
              Js.Unsafe.set select_button "title"
                (Js.string "You must be evaluating code to select an action");
              print_to_output "Caught Failure \"Stop\", restarting init_page...";
              init_page ();
              (* Recursively call init_page to restore button *)
              Lwt.return_unit
          | exn ->
              (* Handle other exceptions *)
              print_to_output ("Unhandled exception: " ^ Printexc.to_string exn);
              Lwt.return_unit))

(* "Main" entry point *)
let () = init_page ()
