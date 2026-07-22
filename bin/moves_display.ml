(* ==================================================
   MOVES_DISPLAY: Interactive move selection UI
   ==================================================
   Manages the display and selection of available moves:
   - highlight_subject: Marks the variable being modified
   - generate_clickables: Creates interactive UI elements
     for each available move
   - get_chosen_move: Waits for user to click and select a move
     (returns Lwt promise for async handling)
*)

open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix

let highlight_subject (move_json_str : string) : unit =
  let previous_highlights =
    Dom_html.document##getElementsByClassName (Js.string "ienv-item-highlighted") 
  in
  while previous_highlights##.length > 0 do
    let item = Js.Opt.get (previous_highlights##item 0)
        (fun () -> assert false) in
    item##.classList##remove (Js.string "ienv-item-highlighted")
  done;
  try
    let json = Yojson.Safe.from_string move_json_str in
    let set_target_highlighted name =
      let target_id = "ienv-item-" ^ name in
      let target_el = Dom_html.getElementById_opt target_id in
      match target_el with
            | Some el -> el##.classList##add (Js.string "ienv-item-highlighted")
            | None -> ()
    in
    match json with
    | `Assoc fields ->
        (match List.assoc_opt "subjectName" fields with
        | Some (`String name) -> set_target_highlighted name
        (* No subject name: Direct style, this move is a "return" and there is
           at least one context so active-ctx exists *)
        | _ -> set_target_highlighted "active-ctx")
    | _ -> ()
  with _ -> ()

let generate_clickables moves =
  let moves_list = Dom_html.getElementById "moves-list" in
  moves_list##.innerHTML := Js.string "";
  List.iteri
    (fun index (id, move) ->
      let checkbox_div = Dom_html.createDiv Dom_html.document in

      let display_label =
        try
          match Yojson.Safe.from_string move with
          | `Assoc fields ->
              (match List.assoc_opt "string" fields with
               | Some (`String s) -> s
               | _ -> move)
          | _ -> move
        with _ -> move
      in

      (* Built from nodes: [display_label] is a pretty-printed move, which may
         contain '<' and '&'. *)
      let radio =
        Dom_html.createInput ~_type:(Js.string "radio") ~name:(Js.string "move")
          Dom_html.document in
      radio##.id := Js.string (Printf.sprintf "move_%d" id);
      radio##.checked := Js.bool (index = 0);

      let label = Dom_html.createLabel Dom_html.document in
      label##setAttribute (Js.string "for") (Js.string (Printf.sprintf "move_%d" id));
      label##.textContent := Js.some (Js.string display_label);

      Dom.appendChild checkbox_div radio;
      Dom.appendChild checkbox_div (Dom_html.document##createTextNode (Js.string " "));
      Dom.appendChild checkbox_div label;

      checkbox_div##.onclick := Dom_html.handler (fun _ ->
        highlight_subject move;
        
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
        highlight_subject first_move_json
    | [] -> ()

(* Index carried by the radio button of a move, encoded in its id as
   "move_<id>" by [generate_clickables]. *)
let move_index_of_radio radio_input =
  match String.split_on_char '_' (Js.to_string radio_input##.id) with
  | [ _; num_str ] -> int_of_string_opt num_str
  | _ -> None

(* The index of the move whose radio button is currently checked, if any. *)
let selected_move_index moves_list =
  let children = Dom.list_of_nodeList moves_list##.childNodes in
  List.fold_left
    (fun acc child ->
      match acc with
      | Some _ -> acc
      | None ->
          let ( let* ) o f = match o with None -> None | Some x -> f x in
          let* element = Js.Opt.to_option (Dom_html.CoerceTo.element child) in
          let* input =
            match element##querySelector (Js.string "input[type='radio']") with
            | exception _ -> None
            | input_opt -> Js.Opt.to_option input_opt in
          let* radio_input = Js.Opt.to_option (Dom_html.CoerceTo.input input) in
          if Js.to_bool radio_input##.checked then move_index_of_radio radio_input
          else None)
    None children

(* The buttons that abandon the interaction. They are optional: the tutorial
   page, for instance, offers no way to load another module. *)
let interrupt_buttons = [ "load-btn"; "stop-btn" ]

let get_chosen_move () =
  let open Interaction_signals in
  match Dom_html.getElementById_opt "select-btn" with
  | None -> Lwt.return Missing_buttons
  | Some select_btn ->
      let interrupts =
        List.filter_map
          (fun id ->
            match Dom_html.getElementById_opt id with
            | None -> None
            | Some btn ->
                Some (Lwt_js_events.click btn >>= fun _ -> Lwt.return Interrupted))
          interrupt_buttons in
      (* [pick] cancels the losing threads: without that, their click listeners
         pile up across rounds. *)
      Lwt.pick
        (( Lwt_js_events.click select_btn >>= fun _ ->
           match Dom_html.getElementById_opt "moves-list" with
           | None -> Lwt.return Missing_buttons
           | Some moves_list -> (
               match selected_move_index moves_list with
               | Some i -> Lwt.return (Chosen i)
               | None -> Lwt.return No_selection) )
         :: interrupts)
