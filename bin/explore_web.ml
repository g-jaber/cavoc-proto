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

(* Generates HTML for the "store" tab (OLD String format) *)
let generate_store_html (store_str : string) : string =
  store_str |> String.split_on_char ';'
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.map (fun line -> Printf.sprintf "<div>%s</div>" line)
  |> String.concat "\n"
  
(* Escapes HTML special characters (&, <, >) for safe rendering. *)
let html_escape (s : string) : string =
  let s = String.concat "&amp;"
    (String.split_on_char '&' s) in
  let s = String.concat "&lt;" (String.split_on_char '<' s) in
  let s = String.concat "&gt;"
    (String.split_on_char '>' s) in
  s

(* Génère le HTML pour le store en format JSON List, en filtrant les doublons de ienv *)
let generate_store_html_from_json (store_json : Yojson.Safe.t) (ienv_json : Yojson.Safe.t option) : string =
  (* 1. Extraire les paires de ienv si disponible pour comparaison *)
  let ienv_pairs =
    match ienv_json with
    | Some (`Assoc fields) -> fields
    | _ -> []
  in

  (* 2. Fonction pour tester les doublons : garde false si pas doublon *)
  let is_duplicate k v =
    match List.assoc_opt k ienv_pairs with
    | Some v_ienv -> v = v_ienv
    | None -> false
  in

  (* 3. Traiter la liste du store *)
  match store_json with
  | `List items ->
      (* Aplatir la liste de dictionnaires en une seule liste de paires *)
      let all_pairs =
        items
        |> List.map (function `Assoc pairs -> pairs | _ -> [])
        |> List.flatten
      in
      
      (* Filtrer les doublons vis-à-vis de ienv *)
      let filtered_pairs =
        List.filter (fun (k, v) -> not (is_duplicate k v)) all_pairs
      in

      (* Générer le HTML *)
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
            (* Utilisation du style 'stack-item' existant pour la cohérence visuelle, avec des couleurs adaptées *)
            Printf.sprintf
              "<div class=\"stack-item\" style='display: flex; flex-direction: row; align-items: baseline; padding: 8px; margin-bottom: 5px;'>\n\
               <span style='color:#fd971f; font-weight: bold; min-width: 50px; margin-right: 10px;'>%s</span>\n\
               <span style='color:#e6db74; font-family: monospace; white-space: pre-wrap; word-break: break-all;'>%s</span>\n\
               </div>"
              (html_escape k) (html_escape v_str))
        |> String.concat "\n"
        |> Printf.sprintf "<div style='padding: 10px; display: flex; flex-direction: column;'>%s</div>"

  | _ -> "<div style='padding: 10px; color: #f92672;'>Invalid store format (expected List of Objects)</div>"


(* [DEBUT] Code pour la mise en évidence dynamique de ienv avec Ace *)

(* La fonction generate_ienv_html est modifiée pour insérer le contenu brut
   dans un élément <pre> avec un ID unique, que Ace prendra ensuite en charge. *)
let generate_ienv_html (ienv_obj : Yojson.Safe.t) : string =
  match ienv_obj with
  | `Assoc fields ->
      let items_html =
        fields
        |> List.mapi (fun i (id, v) ->
            let v_str =
              match v with
              | `String s -> s (* CONTENU BRUT POUR ACE *)
              | _ -> Yojson.Safe.to_string v in
            Printf.sprintf
              (* Ajout de l'attribut id='ienv-item-%s' pour la surbrillance de visibilité *)
              (* L'élément <pre> a un ID unique (ienv-content-%s-%i) pour l'initialisation de Ace *)
              "<div id='ienv-item-%s' class=\"stack-item\" style='position: relative; height: auto;'>\n\
               <span class='ienv-id' style='color:#75715e; margin-right: 10px;'>%s</span>\n\
               <pre id='ienv-content-%s-%i' class='ienv-code'>%s</pre>\n\
            \ </div>"
              id id id i (html_escape v_str))
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


(* Main display configuration function - modifié pour initialiser Ace sur le contenu de ienv *)
let display_conf conf_json : unit =
  (* 1) Affiche le JSON brut dans l’éditeur config (comme avant) *)
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
      
      (* Récupération de ienv OPTIONNEL pour le filtrage du store *)
      let ienv_opt = List.assoc_opt "ienv" fields in

      (* Store : Mise à jour pour gérer le format JSON List et le filtrage *)
      (match List.assoc_opt "store" fields with
      | Some (`List _ as store_json) ->
          (* NOUVEAU : Format liste d'objets -> affichage filtré *)
          let store_html = generate_store_html_from_json store_json ienv_opt in
          update_container "store" store_html
      | Some (`String store_str) ->
          (* ANCIEN : Format chaîne brute *)
          let store_html = generate_store_html store_str in
          update_container "store" store_html
      | _ -> update_container "store" "<div>No store data available</div>");

      (* ienv : Ace coloré pour chaque élément *)
      match ienv_opt with
      | Some ienv_obj ->
          let ienv_html = generate_ienv_html ienv_obj in
          update_container "ienv" ienv_html;
          (* Code OCaml pour initialiser un éditeur Ace sur chaque bloc de code ienv généré *)
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
            (* 1. Coerce generic node to HTML element *)
            let element_opt = Dom_html.CoerceTo.element node in
            let element = Js.Opt.get element_opt (fun () -> failwith "Not an element") in

            let id = Js.to_string element##.id in
            
            (* 2. Handle textContent option *)
            let text_opt = element##.textContent in
            let text = Js.to_string (Js.Opt.get text_opt (fun () -> Js.string "")) in
            
            (* 3. Clear content for Ace *)
            element##.innerHTML := Js.string ""; 

            (* 4. Initialize Ace *)
            let editor = Js.Unsafe.meth_call ace "edit" [|
                Js.Unsafe.inject (Js.string id) |] in
            let session = Js.Unsafe.get editor "session" in
            (* AJOUT : Récupération du renderer pour configurer l'affichage *)
            let renderer = Js.Unsafe.get editor "renderer" in

            (* Configuration de l'éditeur Ace *)
            Js.Unsafe.meth_call editor "setTheme" [|
                Js.Unsafe.inject (Js.string "ace/theme/monokai") |] |> ignore;
            (* AJOUT : Suppression du numéro de ligne (gutter) *)
            Js.Unsafe.meth_call renderer "setShowGutter" [|
                Js.Unsafe.inject Js._false |] |> ignore;
                
            Js.Unsafe.meth_call editor "setReadOnly" [| Js.Unsafe.inject Js._true |] |> ignore;
            Js.Unsafe.meth_call editor "setShowPrintMargin" [|
                Js.Unsafe.inject Js._false |] |> ignore;
            Js.Unsafe.meth_call editor "setHighlightActiveLine" [| Js.Unsafe.inject Js._false |] |> ignore;
            Js.Unsafe.meth_call editor "setHighlightGutterLine" [|
                Js.Unsafe.inject Js._false |] |> ignore;
            Js.Unsafe.meth_call editor "setOptions" [| Js.Unsafe.inject (Js.Unsafe.obj [| ("maxLines", Js.Unsafe.inject (Js.Unsafe.meth_call session "getScreenLength" [||])) |]) |] |> ignore;
            (* Définition du mode et du contenu *)
            Js.Unsafe.meth_call session "setMode" [|
                Js.Unsafe.inject (Js.string "ace/mode/ocaml") |] |> ignore;
            Js.Unsafe.meth_call session "setValue" [| Js.Unsafe.inject (Js.string text) |] |> ignore;
            Js.Unsafe.meth_call editor "clearSelection" [|
              |] |> ignore;
            
            (* Ajuster la hauteur *)
            let new_height = Js.Unsafe.meth_call session "getScreenLength" [|
              |] |> Js.Unsafe.coerce |> Js.to_float in
            let style = element##.style in
            style##.height := Js.string (Printf.sprintf "%fpx" (new_height *. 16.0 +. 4.0));
            Js.Unsafe.meth_call editor "resize" [| |] |> ignore;

            (* Suppression de la bordure et de la marge via Js.Unsafe pour l'objet style du conteneur Ace (qui n'est pas element ici, mais editor.container) *)
            let container = Js.Unsafe.get editor "container" in
            let container_style = Js.Unsafe.get container "style" in
            Js.Unsafe.set container_style "border" (Js.string "none");
            Js.Unsafe.set container_style "margin" (Js.string "0");
            Js.Unsafe.set container_style "padding" (Js.string "0");
            Js.Unsafe.set container_style "backgroundColor" (Js.string "transparent");

          ) code_blocks_list;

      | None -> update_container "ienv" "<div>No ienv data available</div>")
  | _ ->
      print_to_output "Invalid JSON format"

(* [FIN] Code pour la mise en évidence dynamique de ienv avec Ace *)


(* Fonction pour gérer la surbrillance dans l'ienv *)
let highlight_subject (move_json_str : string) : unit =
  (* 1. Nettoyer : Enlever la classe 'ienv-item-highlighted' de tous les éléments *)
  let previous_highlights = 
    Dom_html.document##getElementsByClassName (Js.string "ienv-item-highlighted") 
  in
  (* On boucle à l'envers ou on utilise une boucle while car la collection est 'live' *)
  while previous_highlights##.length > 0 do
    let item = Js.Opt.get (previous_highlights##item 0) 
        (fun () -> assert false) in
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
    | _ -> () (* Échec du parsing JSON *)
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

(* Clear the moves_list. Called when the page is reset*)
let clear_list () : unit =
  let moves_list = Dom_html.getElementById "moves-list" in
  moves_list##.innerHTML := Js.string ""

(* Waits for user to click on one of the buttons and returns the chosen move index *)
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
          (* When button "load" or "stop" are clicked return -1 to make the failure "Stop" happen *)
          (Lwt_js_events.click load_btn >>= fun _ -> Lwt.return (-1));
          (Lwt_js_events.click stop_btn >>= fun _ -> Lwt.return (-1));
        ]

(* Overrides default print functions to redirect to the HTML output div *)
let () =
  Printexc.record_backtrace true;
  Sys_js.set_channel_flusher stdout print_to_output;
  Sys_js.set_channel_flusher stderr print_to_output

let generate_kind_lts () = 
  let open Lts_kind in
  let oplang = RefML in
  let control =  CPS  (* DirectStyle *) in
  let restrictions = [] in
  {oplang; control; restrictions}

let evaluate_code () =
  flush_moves ();
  fetch_editor_content ();
  let kind_lts = generate_kind_lts () in
  let (module OGS_LTS) = Lts_kind.build_lts kind_lts in
  
  (* Parsing et typage *)
  let lexBuffer_code = Lexing.from_string !editor_content in
  let lexBuffer_sig = Lexing.from_string !signature_content in
    
  (* Création de la configuration initiale *)
  let init_conf =
    OGS_LTS.Passive
      (OGS_LTS.lexing_init_pconf lexBuffer_code lexBuffer_sig) in
      
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
        "position: fixed; z-index: 10000; left: 0; top: 0; width: 100%; 
         height: 100%; \
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

      Lwt.return ()

  | IBuild.Stopped ->
      Lwt.return ()



(* Call the parser with the library loaded in the Html*)
let parse_markdown (md : string) : string =
  let marked = Js.Unsafe.get Js.Unsafe.global "marked" in
  Js.to_string (Js.Unsafe.meth_call marked "parse" [| Js.Unsafe.inject (Js.string md) |])

(* Display help*)
let show_help () =
  let modal = Dom_html.getElementById "help-modal" in
  let content_div = Dom_html.getElementById "markdown-content" in
  
  modal##.style##.display := Js.string "block";
  
  (* Get the data within help.md in a asynchronous way *)
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

(* listener event *)
let init_help_events () =
  let help_btn = Dom_html.getElementById "help-btn" in
  let close_span = 
    let elements = Dom_html.document##getElementsByClassName (Js.string "close-btn") in
    Js.Opt.get (elements##item 0) (fun () -> failwith "close-btn not found")
  in
  let modal = Dom_html.getElementById "help-modal" in

  (* User click on the help button *)
  help_btn##.onclick := Dom_html.handler (fun _ -> 
    Lwt.ignore_result (show_help ()); 
    Js._true
  );

(* user click on the closing cross *)
  Js.Opt.iter (Dom_html.CoerceTo.element close_span) (fun el ->
    el##.onclick := Dom_html.handler (fun _ -> close_help ())
  );

  (* user click outside *)
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
  
(* Do page init, creating the callback on the submit button, and managing some button looks*)
(* Pass of an execution : at start : init_page() -> when evaluate button clicked : evaluate_code(), generate_clickable() -> when select button clicked : get_chosen_move() -> 
  highlight_subject() -> when stop or load button clicked : exception Failure "Stop" -> init_page() *)

let rec init_page () =
  init_help_events ();
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
        (fun () -> 
          let%lwt result = evaluate_code () in match result with
          | () -> Lwt.return_unit)
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