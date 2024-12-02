open Js_of_ocaml
open Random

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
  let output_div = Dom_html.getElementById "output" in
  let current_content = Js.to_string (Js.Unsafe.get output_div "innerHTML") in
  let new_content = current_content ^ "<pre>" ^ str ^ "</pre>" in
  Js.Unsafe.set output_div "innerHTML" (Js.string new_content)

(*function wich generate clickable component on the DOM*)
let generate_clickables actions =
  let actions = actions @ [ (-1, "Stop") ] in
  let actions_list = Dom_html.getElementById "actions-list" in
  actions_list##.innerHTML := Js.string "";
  (* Clear existing elements *)
  List.iter
    (fun (id, action) ->
      let checkbox_div = Dom_html.createDiv Dom_html.document in
      checkbox_div##.innerHTML :=
        Js.string
          (Printf.sprintf "<input type='radio' name='action' id='action_%d'> %s"
             id action);
      Dom.appendChild actions_list checkbox_div)
    actions

let get_chosen_action _ =
  print_to_output "Waiting for a click.";
  let%lwt _ =
    Js_of_ocaml_lwt.Lwt_js_events.click (Dom_html.getElementById "actions-list")
  in
  let i = 1 (*Random.int n*) in
  print_to_output @@ "You have clicked but "^ (string_of_int i) ^ " has been chosen!";
  Lwt.return i

(* Overrides default print functions to redirect to the HTML output div *)
let () =
  Printexc.record_backtrace true;
  Sys_js.set_channel_flusher stdout print_to_output;
  Sys_js.set_channel_flusher stderr print_to_output

(* Generates the LTS based on the selected mode and editor content *)

(* Builds and evaluates the OGS LTS based on the provided code content *)
let evaluate_code () =
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
  let show_conf _ = () in
  (* À définir *)
  (*genere les cliquables et les ajoute dans la liste des coups possibles*)
  let show_moves results_list =
    (* Convert the moves list into a list of tuples with dummy ids for demonstration *)
    let actions =
      List.mapi (fun i results_list -> (i, results_list)) results_list in
    generate_clickables actions in
  (*event listener sur les cliquables renvoyant l'index de celui sur lequel l'utilisateur a cliqué *)
  let get_move n =
    print_to_output "In get_move";
    let%lwt i = get_chosen_action n in
    if i > 0 && i <= n then begin
      print_to_output ("Selected action " ^ string_of_int i);
      Lwt.return i
    end
    else begin
      print_to_output ("No button ;(" ^ string_of_int i);
      exit 1
    end in
  IBuild.interactive_build ~show_conf ~show_moves ~get_move init_conf

(* Sets up the event listener for the "Evaluer" button *)
let () =
  self_init ();
  let button = Dom_html.getElementById "submit" in
  Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
      let%lwt _ = Js_of_ocaml_lwt.Lwt_js_events.click button in
      evaluate_code ())
