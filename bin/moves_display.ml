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
    match json with
    | `Assoc fields ->
        (match List.assoc_opt "subjectName" fields with
        | Some (`String name) ->
            let target_id = "ienv-item-" ^ name in
            let target_el = Dom_html.getElementById_opt target_id in
            (match target_el with
            | Some el -> el##.classList##add (Js.string "ienv-item-highlighted")
            | None -> ())
        | _ -> ())
    | _ -> ()
  with _ -> ()

let generate_clickables moves =
  let moves_list = Dom_html.getElementById "moves-list" in
  moves_list##.innerHTML := Js.string "";
  List.iteri
    (fun index (id, move) ->
      let checkbox_div = Dom_html.createDiv Dom_html.document in
      let checked_attr = if index = 0 then " checked" else "" in
      
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

      checkbox_div##.innerHTML :=
        Js.string
          (Printf.sprintf
            "<input type='radio' name='move' id='move_%d'%s> <label \
              for='move_%d'>%s</label>"
            id checked_attr id display_label);

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

let get_chosen_move _ =
  let select_btn_opt = Dom_html.getElementById_opt "select-btn" in
  let load_btn_opt = Dom_html.getElementById_opt "load-btn" in
  let stop_btn_opt = Dom_html.getElementById_opt "stop-btn" in
  match (select_btn_opt, load_btn_opt, stop_btn_opt) with
  | (None, _, _) -> Lwt.return (-2)
  | (_, None, _) -> Lwt.return (-2)
  | (_, _, None) -> Lwt.return (-2)
  | (Some select_btn, Some load_btn, Some stop_btn) ->
      Lwt.choose
        [
          ( Lwt_js_events.click select_btn >>= fun _ ->
            let moves_list_opt = Dom_html.getElementById_opt "moves-list" in
            match moves_list_opt with
            | None -> Lwt.return (-2)
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
