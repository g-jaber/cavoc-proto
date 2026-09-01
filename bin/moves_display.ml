(* ==================================================
   MOVES_DISPLAY: the choice panel
   ==================================================
   The interaction only ever asks the user one kind of question - pick one of
   n alternatives - and this module renders it: the available moves, and the
   configurations a symbolic evaluation split into (Evaluate_code.choose_conf)
   go through the same rows.

   - render_choice_list: fills the panel with clickable rows; clicking a row
     plays it, focusing it previews it (on_focus)
   - wait_choice: waits for the user to click a row, or Stop
   - generate_clickables / get_chosen_move: the move instance, with the
     subject highlight as its preview
*)

open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix

(* The buttons that abandon the current interaction: the single interrupt
   path. Changing scenario or file during a run goes through Stop too
   (front/scenario.js clicks it). *)
let interrupt_buttons = [ "stop-btn" ]

(* The entries of the cards' live panels carry their name as data-name
   (Display_config.entry_html); the subject of the focused move is found by
   it, wherever its card is. Besides the highlight, the entry gets the peek
   class: a collapsed panel still reveals it, so it keeps answering "what am
   I about to call?". *)
let remove_class_everywhere class_name =
  Dom_html.document##querySelectorAll (Js.string ("." ^ class_name))
  |> Dom.list_of_nodeList
  |> List.iter (fun node ->
         Js.Opt.iter (Dom_html.CoerceTo.element node) (fun el ->
             el##.classList##remove (Js.string class_name)))

let highlight_subject (move_json_str : string) : unit =
  remove_class_everywhere "ienv-item-highlighted";
  remove_class_everywhere "peek";
  try
    let json = Yojson.Safe.from_string move_json_str in
    let set_target_highlighted name =
      let selector =
        Printf.sprintf ".ienv-item[data-name=\"%s\"]"
          (Ui_helpers.html_escape name) in
      Dom_html.document##querySelectorAll (Js.string selector)
      |> Dom.list_of_nodeList
      |> List.iter (fun node ->
             Js.Opt.iter (Dom_html.CoerceTo.element node) (fun el ->
                 el##.classList##add (Js.string "ienv-item-highlighted");
                 el##.classList##add (Js.string "peek"))) in
    match json with
    | `Assoc fields -> (
        match List.assoc_opt "subjectName" fields with
        | Some (`String name) when name <> "" -> set_target_highlighted name
        (* No subject name: Direct style, this move is a "return" and there is
           at least one context so active-ctx exists *)
        | _ -> set_target_highlighted "active-ctx")
    | _ -> ()
  with _ -> ()

let click_button id =
  match Dom_html.getElementById_opt id with
  | None -> ()
  | Some el -> ignore (Js.Unsafe.meth_call el "click" [||])

let set_caption text =
  match Dom_html.getElementById_opt "choice-caption" with
  | None -> ()
  | Some el -> el##.textContent := Js.some (Js.string text)

let choice_rows moves_list =
  moves_list##querySelectorAll (Js.string ".choice-row")
  |> Dom.list_of_nodeList
  |> List.filter_map (fun node ->
         Js.Opt.to_option (Dom_html.CoerceTo.element node))

let focus_row row = ignore (Js.Unsafe.meth_call row "focus" [||])

(* The row of the panel that last held the keyboard. The arrow keys and Enter
   only work while a row has focus, and clicking a row is playing it — so a
   click on the panel's chrome (caption, background, hint) is the gesture
   that hands the keyboard back, to this row (render_choice_list wires it). *)
let last_focused_row_id : string option ref = ref None

let focus_remembered_row () =
  let focus_first_row () =
    match Dom_html.getElementById_opt "moves-list" with
    | None -> ()
    | Some moves_list -> (
        match choice_rows moves_list with
        | row :: _ -> focus_row row
        | [] -> ()) in
  match Option.bind !last_focused_row_id Dom_html.getElementById_opt with
  | Some row -> focus_row row
  | None -> focus_first_row ()

(* Moves the focus [delta] rows up or down the panel, for the arrow keys. *)
let move_focus delta =
  match Dom_html.getElementById_opt "moves-list" with
  | None -> ()
  | Some moves_list ->
      let rows = choice_rows moves_list in
      let active_id =
        Js.Opt.case
          Dom_html.document##.activeElement
          (fun () -> None)
          (fun el -> Some (Js.to_string el##.id)) in
      let current =
        match active_id with
        | None -> -1
        | Some id ->
            let rec find i = function
              | [] -> -1
              | row :: rest ->
                  if Js.to_string row##.id = id then i else find (i + 1) rest
            in
            find 0 rows in
      let last = List.length rows - 1 in
      let target = max 0 (min last (current + delta)) in
      match List.nth_opt rows target with
      | None -> ()
      | Some row -> ignore (Js.Unsafe.meth_call row "focus" [||])

(* Fills the choice panel with one clickable row per alternative. Clicking a
   row is choosing it (wait_choice below picks the clicks up); focusing one
   calls [on_focus id], which is where previews plug in - the subject
   highlight for moves, the configuration display for symbolic branches.
   The rows are buttons, so Enter on the focused row is natively a click;
   the arrow keys and Escape are wired here. *)
let render_choice_list ?caption ?(on_focus = fun (_ : int) -> ())
    (rows : (int * string) list) =
  (match caption with Some text -> set_caption text | None -> ());
  let moves_list = Dom_html.getElementById "moves-list" in
  moves_list##.innerHTML := Js.string "";
  last_focused_row_id := None;
  (* Clicking the panel anywhere but on a row hands the keyboard back to the
     rows without playing anything. Assigning the onclick property anew on
     each render keeps the handlers from piling up. *)
  (match Dom_html.getElementById_opt "choice-panel" with
  | None -> ()
  | Some panel ->
      panel##.onclick :=
        Dom_html.handler (fun event ->
            let clicked_row =
              Js.Opt.case event##.target
                (fun () -> false)
                (fun target ->
                  Js.to_bool
                    (target##.classList##contains (Js.string "choice-row")))
            in
            if not clicked_row then focus_remembered_row ();
            Js._true));
  (* Keyboard control while focus is inside the panel. Scoped to the list so
     it never interferes with typing in the editors. *)
  moves_list##.onkeydown :=
    Dom_html.handler (fun e ->
        match e##.keyCode with
        | 38 ->
            move_focus (-1);
            Js._false
        | 40 ->
            move_focus 1;
            Js._false
        | 27 ->
            click_button "stop-btn";
            Js._false
        | _ -> Js._true);
  let first_row = ref None in
  List.iteri
    (fun index (id, label) ->
      let row = Dom_html.createButton Dom_html.document in
      row##.className := Js.string "choice-row";
      row##.id := Js.string (Printf.sprintf "choice_%d" id);
      row##setAttribute (Js.string "data-choice-id")
        (Js.string (string_of_int id));
      (* A text node: [label] is a pretty-printed move, which may contain '<'
         and '&'. *)
      row##.textContent := Js.some (Js.string label);
      ignore
        (Dom.addEventListener row Dom_html.Event.focus
           (Dom_html.handler (fun _ ->
                last_focused_row_id := Some (Js.to_string row##.id);
                on_focus id;
                Js._true))
           Js._false);
      if index = 0 then first_row := Some row;
      Dom.appendChild moves_list row)
    rows;
  (* Focus the first row so the keyboard drives the panel straight away,
     and so its preview shows. *)
  match !first_row with
  | Some row -> ignore (Js.Unsafe.meth_call row "focus" [||])
  | None -> ()

let render_notice ~caption note =
  set_caption caption;
  let moves_list = Dom_html.getElementById "moves-list" in
  moves_list##.innerHTML := Js.string "";
  last_focused_row_id := None;
  let line = Dom_html.createDiv Dom_html.document in
  line##.className := Js.string "panel-empty";
  line##.textContent := Js.some (Js.string note);
  Dom.appendChild moves_list line

(* The id carried by a row, encoded by render_choice_list. *)
let choice_id_of_row row =
  row##getAttribute (Js.string "data-choice-id")
  |> Js.Opt.to_option
  |> fun o -> Option.bind o (fun s -> int_of_string_opt (Js.to_string s))

(* Waits for the user to answer the choice panel: a click on a row (Enter on
   a focused row is a click too) chooses it, a click on an interrupt button
   abandons the interaction. *)
let wait_choice () =
  let open Interaction_signals in
  match Dom_html.getElementById_opt "moves-list" with
  | None -> Lwt.return Missing_buttons
  | Some moves_list ->
      let choices =
        List.filter_map
          (fun row ->
            match choice_id_of_row row with
            | None -> None
            | Some id ->
                Some (Lwt_js_events.click row >>= fun _ -> Lwt.return (Chosen id)))
          (choice_rows moves_list) in
      let interrupts =
        List.filter_map
          (fun id ->
            match Dom_html.getElementById_opt id with
            | None -> None
            | Some btn ->
                Some
                  (Lwt_js_events.click btn >>= fun _ -> Lwt.return Interrupted))
          interrupt_buttons in
      (* [pick] cancels the losing threads: without that, their click listeners
         pile up across rounds. *)
      Lwt.pick (choices @ interrupts)

(* The move instance of the choice panel: rows labelled by the move's pretty
   form, previewed by highlighting its subject in the IEnv panel. *)
let generate_clickables moves =
  let label_of move =
    try
      match Yojson.Safe.from_string move with
      | `Assoc fields -> (
          match List.assoc_opt "string" fields with
          | Some (`String s) -> s
          | _ -> move)
      | _ -> move
    with _ -> move in
  let on_focus id =
    match List.assoc_opt id moves with
    | Some move -> highlight_subject move
    | None -> () in
  render_choice_list ~caption:"Your move — click to play" ~on_focus
    (List.map (fun (id, move) -> (id, label_of move)) moves)

let get_chosen_move () = wait_choice ()
