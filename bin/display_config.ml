(* =========================================================
   DISPLAY_CONFIG: the cards' live panels
   =========================================================
   Renders a passive configuration into the participant card it belongs to:
   the IEnv and Store live panels, and the raw JSON in the Configuration
   tab. *)
(* The panel elements are built by front/common.js and hold the collapsed/Δ/
   full state, so only the strip text and the entries are replaced here. *)

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

(* --- the Δ of the last move ------------------------------------------
   An ienv entry is fresh when its name is new, a store cell when it is new or
   its value changed, both read off the previously committed configuration. *)
(* Contexts only ever grow, so the increment stays small even when the total
   is large. *)

type committed_entries = {
  ienv_pairs: (string * string) list;
  store_pairs: (string * string) list;
}

let committed_baselines : (int * committed_entries) list ref = ref []
let reset_move_deltas () = committed_baselines := []

let baseline_of_card card =
  match List.assoc_opt card !committed_baselines with
  | Some entries -> entries
  | None -> { ienv_pairs= []; store_pairs= [] }

let commit_baseline card entries =
  committed_baselines :=
    (card, entries) :: List.remove_assoc card !committed_baselines

(* --- HTML ----------------------------------------------------------------- *)

let plural n word = if n = 1 then word else word ^ "s"

(* Prepended to the entries when the last move changed nothing, so the Δ state
   of the panel has something to say. *)
let delta_note_html fresh_count =
  if fresh_count = 0 then
    "<div class=\"delta-note\">no change on the last move</div>"
  else ""

(* One entry: a one-line clamped summary, expandable into the full text. *)
(* Only ienv entries are subjects a move can address, so only they carry the
   class Moves_display.highlight_subject targets. *)
let entry_html ~fresh ~name ~item_class ~head_class ~text =
  Printf.sprintf
    "<details class=\"live-entry %s%s\" data-name=\"%s\">\n\
     <summary><span class=\"%s\">%s</span><span \
     class=\"entry-preview\">%s</span></summary>\n\
     <pre class=\"entry-code\">%s</pre>\n\
     </details>"
    item_class
    (if fresh then " fresh" else "")
    (Ui_helpers.html_escape name)
    head_class
    (Ui_helpers.html_escape name)
    (Ui_helpers.html_escape text)
    (Ui_helpers.html_escape text)

(* Writes one live panel of a card, leaving the panel node itself — and with
   it the collapsed/Δ/full state — alone. *)
let update_live_panel ~card ~kind ~strip_text ~entries_html =
  let element_id suffix = Printf.sprintf "card-%d-%s%s" card kind suffix in
  (match Dom_html.getElementById_opt (element_id "-panel") with
  | Some panel -> panel##.classList##add (Js.string "has-data")
  | None -> ());
  (match Dom_html.getElementById_opt (element_id "-strip") with
  | Some strip -> strip##.textContent := Js.some (Js.string strip_text)
  | None -> ());
  match Dom_html.getElementById_opt (element_id "") with
  | Some container -> container##.innerHTML := Js.string entries_html
  | None -> ()

let display_ienv_panel ~card ~baseline ienv_json =
  match ienv_json with
  | None ->
      update_live_panel ~card ~kind:"ienv"
        ~strip_text:"interactive environment — none"
        ~entries_html:"<div class=\"conf-notice\">No ienv data available.</div>";
      []
  | Some ienv_json -> (
      match ienv_entry_pairs ienv_json with
      | None ->
          update_live_panel ~card ~kind:"ienv"
            ~strip_text:"interactive environment — invalid format"
            ~entries_html:
              (Printf.sprintf
                 "<div class=\"conf-notice\">Expected a JSON object (Assoc), \
                  but received:<br><code>%s</code></div>"
                 (Ui_helpers.html_escape
                    (Yojson.Safe.pretty_to_string ienv_json)));
          []
      | Some pairs ->
          let fresh_names =
            List.filter
              (fun (name, _) -> not (List.mem_assoc name baseline.ienv_pairs))
              pairs in
          let entries_html =
            delta_note_html (List.length fresh_names)
            ^ (pairs
              |> List.map (fun (name, text) ->
                  entry_html
                    ~fresh:(List.mem_assoc name fresh_names)
                    ~name ~item_class:"ienv-item" ~head_class:"ienv-id" ~text)
              |> String.concat "\n") in
          let strip_text =
            Printf.sprintf "interactive environment — %d %s · Δ +%d"
              (List.length pairs)
              (plural (List.length pairs) "name")
              (List.length fresh_names) in
          update_live_panel ~card ~kind:"ienv" ~strip_text ~entries_html;
          pairs)

let display_store_panel ~card ~baseline store_json ienv_json =
  match store_json with
  | None ->
      update_live_panel ~card ~kind:"store" ~strip_text:"store — none"
        ~entries_html:
          "<div class=\"conf-notice\">No store data available.</div>";
      []
  | Some store_json ->
      let pairs = store_entry_pairs store_json ienv_json in
      let changed_cells =
        List.filter
          (fun (location, value) ->
            match List.assoc_opt location baseline.store_pairs with
            | Some previous_value -> previous_value <> value
            | None -> true)
          pairs in
      let entries_html =
        delta_note_html (List.length changed_cells)
        ^ (pairs
          |> List.map (fun (location, text) ->
              entry_html
                ~fresh:(List.mem_assoc location changed_cells)
                ~name:location ~item_class:"store-item" ~head_class:"store-key"
                ~text)
          |> String.concat "\n") in
      let strip_text =
        Printf.sprintf "store — %d %s · %d changed" (List.length pairs)
          (if List.length pairs = 1 then "entry" else "entries")
          (List.length changed_cells) in
      update_live_panel ~card ~kind:"store" ~strip_text ~entries_html;
      pairs

(* The Configuration tab is an ACE editor, reached through the instance the
   page stored on the window object (see cavocInitAce in front/common.js). *)
let set_config_editor_text (text : string) : unit =
  let config_editor = Js.Unsafe.get Js.Unsafe.global "configEditor" in
  let session = Js.Unsafe.get config_editor "session" in
  Js.Unsafe.meth_call session "setValue" [| Js.Unsafe.inject (Js.string text) |]
  |> ignore;
  Js.Unsafe.meth_call config_editor "clearSelection" [||] |> ignore

(* Shown in place of a configuration whose branch has no continuation. *)
(* Previews never advance the Δ baselines, and neither does this. *)
let display_terminal_conf ?(card = 0) (reason : string) : unit =
  set_config_editor_text reason;
  let notice =
    Printf.sprintf "<div class=\"conf-notice\">%s</div>"
      (Ui_helpers.html_escape reason) in
  update_live_panel ~card ~kind:"ienv"
    ~strip_text:"interactive environment — no continuation" ~entries_html:notice;
  update_live_panel ~card ~kind:"store" ~strip_text:"store — no continuation"
    ~entries_html:notice

(* The live panels of one card, fed from one component's configuration
   JSON; the composite routes its two sides here, one card each. *)
let display_conf_panels ~card ~preview conf_json : unit =
  match conf_json with
  | `Assoc fields ->
      let ienv_json = List.assoc_opt "ienv" fields in
      let store_json = List.assoc_opt "store" fields in
      let baseline = baseline_of_card card in
      let ienv_pairs = display_ienv_panel ~card ~baseline ienv_json in
      let store_pairs =
        display_store_panel ~card ~baseline store_json ienv_json in
      if not preview then commit_baseline card { ienv_pairs; store_pairs }
  | _ -> Ui_helpers.print_to_output "Invalid JSON format"

let display_conf ?(card = 0) ?(preview = false) conf_json : unit =
  set_config_editor_text (Yojson.Safe.pretty_to_string conf_json);
  display_conf_panels ~card ~preview conf_json

(* --- the composite: two cards and the gutters ------------------------------
   A composite passive configuration serializes as {left, right, pos,
   syncState}, whose four fields feed the two cards and the two gutters. *)

let set_inner_html element_id html =
  match Dom_html.getElementById_opt element_id with
  | None -> ()
  | Some element -> element##.innerHTML := Js.string html

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

(* The composite position, in the first slot of the public gutter's head: a
   P n · O m strip expandable into the two name lists. *)
let display_composite_position pos_json =
  match base_position_json pos_json with
  | Some (`Assoc fields) ->
      let entries key =
        match List.assoc_opt key fields with
        | Some ctx_json -> namectx_json_entries ctx_json
        | None -> [] in
      let entriesP = entries "namectxP" in
      let entriesO = entries "namectxO" in
      let entry_line (name, ty) =
        Printf.sprintf "<div class=\"gutter-entry\">%s : %s</div>"
          (Ui_helpers.html_escape name)
          (Ui_helpers.html_escape ty) in
      let name_block label = function
        | [] -> ""
        | entries ->
            Printf.sprintf "<div class=\"gutter-block\">%s</div>%s" label
              (String.concat "" (List.map entry_line entries)) in
      set_inner_html "gutter-position"
        (Printf.sprintf
           "<details><summary>position — P %d · O %d</summary>%s%s</details>"
           (List.length entriesP) (List.length entriesO)
           (name_block "P (you may call)" entriesP)
           (name_block "O (you introduced)" entriesO))
  | _ -> ()

(* The shared context, in the second slot of the public gutter's head: one
   row per shared entity, one column per component, the left one first. *)
(* The two sides of the synchronization state have opposite introducers, so
   one of them is read backwards here. *)
let display_shared_spans sync_json =
  match sync_json with
  | `Assoc fields ->
      let table_pairs key =
        match List.assoc_opt key fields with
        | Some (`List rows) ->
            List.filter_map
              (function
                | `List [ key_name; value_name ] ->
                    Some (json_text key_name, json_text value_name)
                | _ -> None)
              rows
        | _ -> [] in
      (* shared_left sends a client O-name to a provider P-name, shared_right
         being its mirror. *)
      let provided_by_left = table_pairs "sharedLeft" in
      let provided_by_right = table_pairs "sharedRight" in
      let pairs =
        List.map
          (fun (client_name, provider_name) -> (provider_name, client_name))
          provided_by_left
        @ provided_by_right in
      let row_html (provider_name, client_name) =
        Printf.sprintf
          "<div class=\"span-row\"><span class=\"span-name\">%s</span><span \
           class=\"span-arrow\">↔</span><span \
           class=\"span-name\">%s</span></div>"
          (Ui_helpers.html_escape provider_name)
          (Ui_helpers.html_escape client_name) in
      let count = List.length pairs in
      let identity =
        count > 0
        && List.for_all
             (fun (provider_name, client_name) -> provider_name = client_name)
             pairs in
      set_inner_html "gutter-shared"
        (Printf.sprintf
           "<details><summary>shared context — %d %s%s</summary>%s</details>"
           count (plural count "name")
           (if identity then " · identity" else "")
           (String.concat "" (List.map row_html pairs)))
  | _ -> ()

let display_composite_conf ?(preview = false) conf_json : unit =
  set_config_editor_text (Yojson.Safe.pretty_to_string conf_json);
  match conf_json with
  | `Assoc fields -> (
      let side key card =
        match List.assoc_opt key fields with
        | Some half_json -> display_conf_panels ~card ~preview half_json
        | None -> () in
      side "left" 0;
      side "right" 1;
      (match List.assoc_opt "pos" fields with
      | Some pos_json -> display_composite_position pos_json
      | None -> ());
      match List.assoc_opt "syncState" fields with
      | Some sync_json -> display_shared_spans sync_json
      | None -> ())
  | _ -> Ui_helpers.print_to_output "Invalid JSON format"
