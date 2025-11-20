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

let html_escape (s : string) : string =
  let s = String.concat "&amp;" (String.split_on_char '&' s) in
  let s = String.concat "&lt;" (String.split_on_char '<' s) in
  let s = String.concat "&gt;" (String.split_on_char '>' s) in
  s

(* Échappe un caractère HTML *)
let escape_char buf c =
  match c with
  | '&' -> Buffer.add_string buf "&amp;"
  | '<' -> Buffer.add_string buf "&lt;"
  | '>' -> Buffer.add_string buf "&gt;"
  | _   -> Buffer.add_char buf c

(* Palette Monokai *)
let color_keyword   = "#f92672"
let color_string    = "#e6db74"
let color_comment   = "#75715e"
let color_number    = "#ae81ff"

let ocaml_keywords =
  [ "let"; "in"; "fun"; "function"; "match"; "with"; "type"; "module";
    "open"; "if"; "then"; "else"; "rec"; "and"; "assert"; "try"; "catch";
    "struct"; "sig"; "end"; "val" ]

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let highlight_ocaml (s : string) : string =
  let len = String.length s in
  let buf = Buffer.create (len * 2) in

  (* états du mini-lexer *)
  let rec normal i =
    if i >= len then ()
    else
      match s.[i] with
      | '"' ->
          (* début de chaîne *)
          Buffer.add_string buf ("<span style='color:" ^ color_string ^ ";'>");
          escape_char buf '"';
          string (i + 1)
      | '(' when i + 1 < len && s.[i + 1] = '*' ->
          (* début de commentaire *)
          Buffer.add_string buf ("<span style='color:" ^ color_comment ^ ";font-style:italic;'>");
          escape_char buf '(';
          escape_char buf '*';
          comment (i + 2)
      | c when is_digit c ->
          (* nombre *)
          let j = ref (i + 1) in
          while !j < len && is_digit s.[!j] do
            incr j
          done;
          Buffer.add_string buf ("<span style='color:" ^ color_number ^ ";'>");
          for k = i to !j - 1 do escape_char buf s.[k] done;
          Buffer.add_string buf "</span>";
          normal !j
      | c when is_ident_char c ->
          (* ident ou mot-clé *)
          let j = ref (i + 1) in
          while !j < len && is_ident_char s.[!j] do
            incr j
          done;
          let word = String.sub s i (!j - i) in
          if List.mem word ocaml_keywords then
            Buffer.add_string buf
              (Printf.sprintf "<span style='color:%s;font-weight:bold;'>%s</span>"
                 color_keyword word)
          else
            (* ident normal *)
            for k = i to !j - 1 do escape_char buf s.[k] done;
          normal !j
      | c ->
          escape_char buf c;
          normal (i + 1)

  and string i =
    if i >= len then (
      (* on ferme quand même la span si chaîne non terminée *)
      Buffer.add_string buf "</span>"
    ) else
      let c = s.[i] in
      match c with
      | '"' ->
          escape_char buf '"';
          Buffer.add_string buf "</span>";
          normal (i + 1)
      | '\\' when i + 1 < len ->
          (* séquence d’échappement simple *)
          escape_char buf '\\';
          escape_char buf s.[i + 1];
          string (i + 2)
      | _ ->
          escape_char buf c;
          string (i + 1)

  and comment i =
    if i >= len then (
      Buffer.add_string buf "</span>"
    ) else if i + 1 < len && s.[i] = '*' && s.[i + 1] = ')' then (
      escape_char buf '*';
      escape_char buf ')';
      Buffer.add_string buf "</span>";
      normal (i + 2)
    ) else (
      escape_char buf s.[i];
      comment (i + 1)
    )
  in

  normal 0;
  Buffer.contents buf

let generate_ienv_html (ienv_obj : Yojson.Safe.t) : string =
  match ienv_obj with
  | `Assoc fields ->
      let items_html =
        fields
        |> List.map (fun (id, v) ->
            let v_str =
              match v with
              | `String s -> highlight_ocaml s (* colorisation OCaml *)
              | _ -> html_escape (Yojson.Safe.to_string v) in
            Printf.sprintf
              (* Ajout de l'attribut id='ienv-item-%s' ici vvv *)
              "<div id='ienv-item-%s' class=\"stack-item\" style='color:#f8f8f2;'>\n\
              \  <span style='color:#75715e; margin-right: 10px;'>%s</span>\n\
               <pre style='display:inline; margin:0;'>%s</pre>\n\
              \ </div>"
              id id v_str)
        |> String.concat "\n" in
      Printf.sprintf
        "<div style='padding: 20px; height: 100%%; overflow-y: auto; \
         background:#272822; font-family:monospace;'>\n\
        \  <h3 style='color: #a6e22e; margin-top: 0; margin-bottom: 20px;'>\n\
        \    Interactive Environment\n\
        \  </h3>\n\
        \  <div style='display: flex; flex-direction: column; gap: 8px;'>\n\
        \    %s\n\
        \  </div>\n\
        \ </div>"
        items_html
  | _ ->
      Printf.sprintf
        "<div style='padding: 20px; color: #e74c3c; background:#272822; \
         font-family:monospace;'>\n\
        \  <h3 style='color: #e74c3c; margin-top: 0;'>✗ Format invalide</h3>\n\
        \  <p>Expected a JSON object (Assoc), but received something else.</p>\n\
        \  <div style='background: #2d2e27; padding: 15px; border-radius: 5px; \
         margin-top: 15px; overflow-x: auto; color:#f8f8f2;'>\n\
        \    <code>%s</code>\n\
        \  </div>\n\
        \ </div>"
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

(* Fonction pour gérer la surbrillance dans l'ienv *)
let highlight_subject (move_json_str : string) : unit =
  (* 1. Nettoyer : Enlever la classe 'ienv-item-highlighted' de tous les éléments *)
  let previous_highlights = 
    Dom_html.document##getElementsByClassName (Js.string "ienv-item-highlighted") 
  in
  (* On boucle à l'envers ou on utilise une boucle while car la collection est 'live' *)
  while previous_highlights##.length > 0 do
    let item = Js.Opt.get (previous_highlights##item 0) (fun () -> assert false) in
    item##.classList##remove (Js.string "ienv-item-highlighted")
  done;

  (* 2. Parser le JSON pour trouver le subjectName *)
  try
    let json = Yojson.Safe.from_string move_json_str in
    match json with
    | `Assoc fields ->
        (match List.assoc_opt "subjectName" fields with
        | Some (`String name) ->
            (* 3. Trouver l'élément et ajouter la classe *)
            let target_id = "ienv-item-" ^ name in
            let target_el = Dom_html.getElementById_opt target_id in
            (match target_el with
            | Some el -> el##.classList##add (Js.string "ienv-item-highlighted")
            | None -> ()) (* L'élément n'existe pas dans l'ienv actuel *)
        | _ -> ()) (* Pas de subjectName ou format incorrect *)
    | _ -> ()
  with _ -> () (* Échec du parsing JSON *)

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
             
      (* --- AJOUT : Gestionnaire d'événement au clic --- *)
      (* On utilise Lwt_js_events ou directement le DOM onclick *)
      checkbox_div##.onclick := Dom_html.handler (fun _ ->
        highlight_subject move; (* Appel de notre fonction magique *)
       
        
        (* Force la sélection du radio button si on clique sur la div (UX bonus) *)
        let input = checkbox_div##querySelector (Js.string "input") in
        Js.Opt.iter input (fun node -> 
           let input_el = Dom_html.CoerceTo.input node in
           Js.Opt.iter input_el (fun inp -> inp##.checked := Js._true)
        );
        
        Js._true
      );
      Dom.appendChild moves_list checkbox_div)
    moves;
     
    match moves with
    | (_, first_move_json) :: _ -> 
        (* Puisque le premier élément (index 0) est checked par défaut, 
          on applique son highlight tout de suite *)
        highlight_subject first_move_json
    | [] -> ()

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

let evaluate_code () =
  flush_moves ();
  fetch_editor_content ();
  (* Initialisation des modules du langage *)
  let module OpLang = Refml.RefML.WithAVal (Util.Monad.ListB) in
  let module CpsLang = Lang.Cps.MakeComp (OpLang) () in
  let module IntLang = Lang.Interactive.Make (CpsLang) in
  let module TypingLTS = Ogs.Typing.Make (IntLang) in
  let module OGS_LTS = Ogs.Ogslts.Make (IntLang) (TypingLTS) in
  
  (* Parsing et typage *)
  let lexBuffer_code = Lexing.from_string !editor_content in
  let lexBuffer_sig = Lexing.from_string !signature_content in
  let (interactive_env, store, name_ctxP, name_ctxO) =
    IntLang.get_typed_ienv lexBuffer_code lexBuffer_sig in
    
  (* Création de la configuration initiale *)
  let init_conf =
    OGS_LTS.Passive
      (OGS_LTS.init_pconf store interactive_env name_ctxP name_ctxO) in
      
  (* Création du module de construction interactif *)
  let module IBuild = Lts.Interactive_build.Make (OGS_LTS) in
  
  (* Callbacks pour l'interface *)
  let show_move move = add_move move in
  let show_conf conf : unit = display_conf conf in

  let show_moves_list (json_list : Yojson.Safe.t list) =
    let display_of (v : Yojson.Safe.t) = Yojson.Safe.pretty_to_string v in
    let id_of i (v : Yojson.Safe.t) =
      match v with
      | `Assoc fields -> (
          match List.assoc_opt "id" fields with Some (`Int n) -> n | _ -> i)
      | _ -> i in
    let moves = List.mapi (fun i v -> (id_of i v, display_of v)) json_list in
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

  (* Lancement de la boucle interactive avec gestion du résultat *)
  (*Le css est généré ici direct PEUT ETRE A CHANGER ?!*)
  match%lwt IBuild.interactive_build ~show_move ~show_conf ~show_moves_list ~get_move init_conf with
  | IBuild.Success ->
      (* 1. Création des éléments HTML *)
      let doc = Dom_html.document in
      let modal = Dom_html.createDiv doc in
      let content = Dom_html.createDiv doc in
      let btn = Dom_html.createButton doc in

      (* 2. Style du Modal (fond sombre transparent qui couvre tout) *)
      modal##.style##.cssText := Js.string 
        "position: fixed; z-index: 10000; left: 0; top: 0; width: 100%; height: 100%; \
         background-color: rgba(0,0,0,0.7); display: flex; align-items: center; justify-content: center;";

      (* 3. Style et contenu de la boîte de dialogue *)
      content##.style##.cssText := Js.string 
        "background-color: #272822; border: 2px solid #a6e22e; border-radius: 8px; \
         padding: 30px; text-align: center; color: #f8f8f2; font-family: monospace; \
         box-shadow: 0 5px 15px rgba(0,0,0,0.5); min-width: 300px;";
      
      content##.innerHTML := Js.string 
        "<h2 style='color: #a6e22e; margin-top: 0;'>🏆 SUCCÈS !</h2>\
         <p style='font-size: 1.2em; margin: 20px 0;'>Vous avez déclenché un failwith.</p>";

      (* 4. Configuration du bouton Reset *)
      btn##.textContent := Js.some (Js.string "Recharger / Reset");
      
      btn##.style##.cssText := Js.string 
        "background-color: #a6e22e; color: #272822; border: none; padding: 12px 24px; \
         font-size: 16px; font-weight: bold; border-radius: 5px; cursor: pointer; \
         transition: background 0.3s;";
      
      (* Effet hover simple (optionnel, géré ici par JS direct pour simplifier) *)
      btn##.onmouseover := Dom_html.handler (fun _ -> 
        btn##.style##.backgroundColor := Js.string "#32b568"; Js._true);
      btn##.onmouseout := Dom_html.handler (fun _ -> 
        btn##.style##.backgroundColor := Js.string "#a6e22e"; Js._true);

      (* 5. Action du bouton : Recharger la page *)
      btn##.onclick := Dom_html.handler (fun _ ->
        Dom_html.window##.location##reload;
        Js._true
      );

      (* 6. Assemblage et ajout au document *)
      Dom.appendChild content btn;
      Dom.appendChild modal content;
      Dom.appendChild doc##.body modal;

      Lwt.return_unit

  | IBuild.Stopped ->
      Lwt.return_unit

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
