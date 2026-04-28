(* =========================================
   EVALUATE CODE: Main interactive evaluation loop
   =========================================
   Orchestrates the complete evaluation workflow:
   - evaluate_code: Main entry point that:
     * Fetches code from editors
     * Initializes LTS with chosen configuration
     * Coordinates callbacks for displaying moves,
       configuration, and move history
     * Implements the step-by-step evaluation loop
   - Integrates all other modules to provide full
     interactive evaluation experience
*)

open Js_of_ocaml
open Lwt.Infix

let evaluate_code () =
  Moves_manager.flush_moves ();
  Editor_manager.fetch_editor_content ();
  let kind_lts = Lts_config.generate_kind_lts () in
  let (module OGS_LTS) = Lts_kind.build_lts kind_lts in
  
  let lexBuffer_code = Lexing.from_string !Editor_manager.editor_content in
  let lexBuffer_sig = Lexing.from_string !Editor_manager.signature_content in

  Lexing.set_filename lexBuffer_code !Editor_manager.editor_filename ;
  Lexing.set_filename lexBuffer_sig !Editor_manager.signature_filename ;
    
  let init_conf =
    OGS_LTS.Passive
      (OGS_LTS.lexing_init_pconf lexBuffer_code lexBuffer_sig) in
      
  let module IBuild = Lts.Interactive_build.Make (OGS_LTS) in
  
  let show_move move = Moves_manager.add_move move in
  let show_conf conf : unit = Display_config.display_conf conf in

  let show_moves_list (json_list : Yojson.Safe.t list) =
    let display_of (v : Yojson.Safe.t) = Yojson.Safe.pretty_to_string v in
    let id_of i (v : Yojson.Safe.t) =
      match v with
      | `Assoc fields -> (
          match List.assoc_opt "id" fields with Some (`Int n) -> n | _ -> i)
      | _ -> i in
    let moves = List.mapi (fun i v -> (id_of i v, display_of v)) json_list in
    Moves_display.generate_clickables moves in

  let get_move n =
    let n = n + 1 in
    let%lwt i = Moves_display.get_chosen_move n in
    Ui_helpers.print_to_output ("Chosen move index: " ^ string_of_int i);
    match i with
    | i when i >= 0 && i < n -> Lwt.return i
    | -1 ->
        Moves_manager.clear_list ();
        Lwt.fail (Failure "Stop")
    | -2 -> Lwt.fail (Failure "No button")
    | _ ->
        Ui_helpers.print_to_output "error : unknown";
        Lwt.fail (Failure "Unknown error") in

  match%lwt IBuild.interactive_build ~show_move ~show_conf ~show_moves_list ~get_move init_conf with
  | IBuild.Success ->
      let () = Js.Unsafe.global##onSuccess [||] in

      Lwt.return 1

  | IBuild.Stopped ->
      Lwt.return 0
