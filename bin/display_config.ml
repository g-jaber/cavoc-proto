(* =========================================================
   DISPLAY_CONFIG: the cards' live panels
   =========================================================
   Renders a passive configuration into the participant card it belongs to:
   the IEnv and Store live panels, and the raw JSON in the Configuration
   tab. *)
(* The panel elements are built by front/common.js, so only their entries are
   replaced here. *)

open Js_of_ocaml

let json_text = function `String s -> s | v -> Yojson.Safe.to_string v

let normalize_ienv = function
  (* CPS: continuations are values (named). *)
  | `Assoc _ as a -> a
  (* Direct style: values are named (fields)
                   execution contexts (stack = exprs) are unamed *)
  | `List [ `Assoc fields; `List exprs ] -> begin
      match exprs with
      | [] -> `Assoc fields (* no execution context *)
      | active :: rest ->
          let active = ("active-ctx", active) in
          (* Reverse stack order to keep numbers stable *)
          let ctx = List.rev rest in
          let ctx = List.mapi (fun i v -> ("stack" ^ string_of_int i, v)) ctx in
          `Assoc ((active :: fields) @ ctx)
    end
  (* An unexpected shape is returned as None: display_conf then falls through
     to its error panel rather than crashing the interaction. *)
  | _ -> `Null

(* The named entries of an ienv, as (name, printed value) pairs; None when the
   JSON has an unexpected shape. *)
let ienv_entry_pairs (ienv_json : Yojson.Safe.t) : (string * string) list option
    =
  match normalize_ienv ienv_json with
  | `Assoc fields -> Some (List.map (fun (id, v) -> (id, json_text v)) fields)
  | _ -> None

(* The cells of the store, as (name, printed value) pairs: the private
   bindings, the heap cells, then the symbolic path when non-empty. *)
let store_entry_pairs (store_json : Yojson.Safe.t)
    (ienv_json : Yojson.Safe.t option) : (string * string) list =
  let ienv_names =
    match Option.map ienv_entry_pairs ienv_json with
    | Some (Some pairs) -> List.map fst pairs
    | _ -> [] in
  match store_json with
  | `Assoc fields ->
      let entries key =
        match List.assoc_opt key fields with
        | Some (`Assoc pairs) -> pairs
        | _ -> [] in
      let private_bindings =
        List.filter
          (fun (name, _) -> not (List.mem name ienv_names))
          (entries "valenv") in
      let symbolic_path =
        List.filter (fun (_, v) -> v <> `List []) (entries "symbolic_ctx") in
      List.map
        (fun (name, value) -> (name, json_text value))
        (private_bindings @ entries "heap" @ symbolic_path)
  | _ -> []

(* --- HTML ----------------------------------------------------------------- *)

(* One entry: its name, then its full text. *)
(* Only ienv entries are subjects a move can address, so only they carry the
   class Moves_display.highlight_subject targets. *)
let entry_html ~name ~item_class ~head_class ~text =
  Printf.sprintf
    "<div class=\"live-entry %s\" data-name=\"%s\"><span \
     class=\"%s\">%s</span><span class=\"entry-text\">%s</span></div>"
    item_class
    (Ui_helpers.html_escape name)
    head_class
    (Ui_helpers.html_escape name)
    (Ui_helpers.html_escape text)

(* Writes the entries of one live panel of a card. *)
let update_live_panel ~card ~kind ~entries_html =
  let element_id suffix = Printf.sprintf "card-%d-%s%s" card kind suffix in
  (match Dom_html.getElementById_opt (element_id "-panel") with
  | Some panel -> panel##.classList##add (Js.string "has-data")
  | None -> ());
  match Dom_html.getElementById_opt (element_id "") with
  | Some container -> container##.innerHTML := Js.string entries_html
  | None -> ()

let display_ienv_panel ~card ienv_json =
  match ienv_json with
  | None ->
      update_live_panel ~card ~kind:"ienv"
        ~entries_html:"<div class=\"conf-notice\">No ienv data available.</div>"
  | Some ienv_json -> (
      match ienv_entry_pairs ienv_json with
      | None ->
          update_live_panel ~card ~kind:"ienv"
            ~entries_html:
              (Printf.sprintf
                 "<div class=\"conf-notice\">Expected a JSON object (Assoc), \
                  but received:<br><code>%s</code></div>"
                 (Ui_helpers.html_escape
                    (Yojson.Safe.pretty_to_string ienv_json)))
      | Some pairs ->
          let entries_html =
            pairs
            |> List.map (fun (name, text) ->
                entry_html ~name ~item_class:"ienv-item" ~head_class:"ienv-id"
                  ~text)
            |> String.concat "\n" in
          update_live_panel ~card ~kind:"ienv" ~entries_html)

let display_store_panel ~card store_json ienv_json =
  match store_json with
  | None ->
      update_live_panel ~card ~kind:"store"
        ~entries_html:
          "<div class=\"conf-notice\">No store data available.</div>"
  | Some store_json ->
      let entries_html =
        store_entry_pairs store_json ienv_json
        |> List.map (fun (location, text) ->
            entry_html ~name:location ~item_class:"store-item"
              ~head_class:"store-key" ~text)
        |> String.concat "\n" in
      update_live_panel ~card ~kind:"store" ~entries_html

(* The Configuration tab is an ACE editor, reached through the instance the
   page stored on the window object (see cavocInitAce in front/common.js). *)
let set_config_editor_text (text : string) : unit =
  let config_editor = Js.Unsafe.get Js.Unsafe.global "configEditor" in
  let session = Js.Unsafe.get config_editor "session" in
  Js.Unsafe.meth_call session "setValue" [| Js.Unsafe.inject (Js.string text) |]
  |> ignore;
  Js.Unsafe.meth_call config_editor "clearSelection" [||] |> ignore

(* Shown in place of a configuration whose branch has no continuation. *)
let display_terminal_conf ?(card = 0) (reason : string) : unit =
  set_config_editor_text reason;
  let notice =
    Printf.sprintf "<div class=\"conf-notice\">%s</div>"
      (Ui_helpers.html_escape reason) in
  update_live_panel ~card ~kind:"ienv" ~entries_html:notice;
  update_live_panel ~card ~kind:"store" ~entries_html:notice

(* The live panels of one card, fed from one component's configuration
   JSON; the composite routes its two sides here, one card each. *)
let display_conf_panels ~card conf_json : unit =
  match conf_json with
  | `Assoc fields ->
      let ienv_json = List.assoc_opt "ienv" fields in
      let store_json = List.assoc_opt "store" fields in
      display_ienv_panel ~card ienv_json;
      display_store_panel ~card store_json ienv_json
  | _ -> Ui_helpers.print_to_output "Invalid JSON format"

let display_conf ?(card = 0) conf_json : unit =
  set_config_editor_text (Yojson.Safe.pretty_to_string conf_json);
  display_conf_panels ~card conf_json

(* --- the composite: two cards and the play panel ------------------------------
   The client left and the module right, each {strategy, position,
   renaming} with its position a pair {left, right}; either split's copy of
   the position of G2 is the shared context. *)

let set_inner_html element_id html =
  match Dom_html.getElementById_opt element_id with
  | None -> ()
  | Some element -> element##.innerHTML := Js.string html

let field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

(* The name/type entries of a name context's JSON, whose {name: type} objects
   are nested in lists by the aggregate structure. *)
let rec namectx_json_entries : Yojson.Safe.t -> (string * string) list =
  function
  | `Assoc fields -> List.map (fun (name, ty) -> (name, json_text ty)) fields
  | `List items -> List.concat_map namectx_json_entries items
  | _ -> []

(* The base typing position inside a position's JSON, the enforcement wrappers
   serializing as lists around it. *)
let rec base_position_json = function
  | `Assoc fields as json when List.mem_assoc "namectxP" fields -> Some json
  | `List items -> List.find_map base_position_json items
  | _ -> None

(* A position in a play panel's head: a P n · O m strip expandable into the two
   name lists. *)
let display_position ~element_id ~title ~labelP ~labelO pos_json =
  match base_position_json pos_json with
  | Some (`Assoc fields) ->
      let entries key =
        match List.assoc_opt key fields with
        | Some ctx_json -> namectx_json_entries ctx_json
        | None -> [] in
      let entriesP = entries "namectxP" in
      let entriesO = entries "namectxO" in
      let entry_line (name, ty) =
        Printf.sprintf "<div class=\"play-panel-entry\">%s : %s</div>"
          (Ui_helpers.html_escape name)
          (Ui_helpers.html_escape ty) in
      let name_block label = function
        | [] -> ""
        | entries ->
            Printf.sprintf "<div class=\"play-panel-block\">%s</div>%s" label
              (String.concat "" (List.map entry_line entries)) in
      set_inner_html element_id
        (Printf.sprintf
           "<details><summary>%s — P %d · O %d</summary>%s%s</details>" title
           (List.length entriesP) (List.length entriesO)
           (name_block labelP entriesP)
           (name_block labelO entriesO))
  | _ -> ()

(* The composite position, in the first slot of the play panel's head. *)
let display_composite_position =
  display_position ~element_id:"play-panel-position" ~title:"position"
    ~labelP:"P (you may call)" ~labelO:"O (you introduced)"

(* The position of G2, in the second slot of the play panel's head, read
   in the module's polarity. *)
let display_shared_position =
  display_position ~element_id:"play-panel-shared" ~title:"shared context"
    ~labelP:"provided by the module" ~labelO:"introduced by the client"

let display_composite_conf conf_json : unit =
  set_config_editor_text (Yojson.Safe.pretty_to_string conf_json);
  match conf_json with
  | `Assoc _ ->
      let split side = Option.bind (field "strategy" conf_json) (field side) in
      (* The page lays the module's card first. *)
      let card split_json card =
        match Option.bind split_json (field "strategy") with
        | Some component_json ->
            display_conf_panels ~card component_json
        | None -> () in
      card (split "right") 0;
      card (split "left") 1;
      Option.iter display_composite_position (field "position" conf_json);
      Option.iter display_shared_position
        (Option.bind
           (Option.bind (split "right") (field "position"))
           (field "left"))
  | _ -> Ui_helpers.print_to_output "Invalid JSON format"
