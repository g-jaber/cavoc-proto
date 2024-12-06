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

let display_conf conf : unit =
  (* Retrieve the ACE config editor instance from the global JavaScript context *)
  let config_editor = Js.Unsafe.get Js.Unsafe.global "configEditor_instance" in
  (* Access the editor's session and use session.setValue to update the content *)
  let session = Js.Unsafe.get config_editor "session" in
  Js.Unsafe.meth_call session "setValue" [| Js.Unsafe.inject (Js.string conf) |]
  |> ignore;
  (* Clear any selection to ensure clean display *)
  Js.Unsafe.meth_call config_editor "clearSelection" [||] |> ignore

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
                    let action_label =
                      Js.to_string
                        (Js.Opt.get element##.textContent (fun () ->
                             assert false)) in
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
                                  add_action action_label;
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
  let module CpsLang = Lang.Cps.MakeComp (OpLang) in
  let module IntLang = Lang.Interactive.Make (CpsLang) in
  let module Int = Lts.Interactive.Make (IntLang) in
  let module OGS_LTS = Ogs.Ogslts.Make (Int) in
  let lexBuffer_code = Lexing.from_string !editor_content in
  let lexBuffer_sig = Lexing.from_string !signature_content in
  let (interactive_env, store, name_ctxP, name_ctxO) =
    OGS_LTS.Int.IntLang.get_typed_ienv lexBuffer_code lexBuffer_sig in
  let init_conf =
    OGS_LTS.Passive
      (OGS_LTS.init_pconf store interactive_env name_ctxP name_ctxO) in
  let module IBuild = Lts.Interactive_build.Make (OGS_LTS) in
  let show_conf conf : unit = display_conf conf in
  (*genere les cliquables et les ajoute dans la liste des coups possibles*)
  let show_moves results_list =
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
  IBuild.interactive_build ~show_conf ~show_moves ~get_move init_conf

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
