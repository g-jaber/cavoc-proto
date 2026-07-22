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
open Js_of_ocaml_lwt
open Lwt.Infix

module MyLwt = struct
  type 'a m = 'a Lwt.t

  let return = Lwt.return
  let ( let* ) = Lwt.bind
end

let show_move move = Moves_manager.add_move move
let show_conf conf : unit = Display_config.display_conf conf

let show_moves_list (json_list : Yojson.Safe.t list) =
  let display_of (v : Yojson.Safe.t) = Yojson.Safe.pretty_to_string v in
  let id_of i (v : Yojson.Safe.t) =
    match v with
    | `Assoc fields -> (
        match List.assoc_opt "id" fields with Some (`Int n) -> n | _ -> i)
    | _ -> i in
  let moves = List.mapi (fun i v -> (id_of i v, display_of v)) json_list in
  Moves_display.generate_clickables moves

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
      Lwt.fail (Failure "Unknown error")

let choose_conf confs =
  let nconf = List.length confs in

  (* avoid prompting the user everytime the LTS stops *)
  if nconf = 1 then Lwt.return 0 else

  let load_btn   = Dom_html.getElementById "load-btn" in
  let stop_btn   = Dom_html.getElementById "stop-btn" in
  let accept_btn = Dom_html.getElementById "conf-accept" in
  let prev_btn   = Dom_html.getElementById "conf-prev" in
  let next_btn   = Dom_html.getElementById "conf-next" in

  Ui_helpers.set_button_enabled "select-btn" false ;
  show_moves_list [] ;

  let cur = ref 0 in
  let has_next () = !cur < nconf - 1 in
  let has_prev () = !cur > 0 in

  let update () =
    Ui_helpers.set_button_enabled "conf-prev" (has_prev ()) ;
    Ui_helpers.set_button_enabled "conf-next" (has_next ()) ;
    Ui_helpers.set_button_enabled "conf-accept" true ;

    match List.nth confs !cur with
    | Some conf -> show_conf conf
    | None ->
        Js.Unsafe.meth_call Js.Unsafe.global "alert"
          [| Js.Unsafe.inject @@ Js.string "This configuration causes the opponent to quit the game" |]
  in

  update () ;

  (* Afaik it is not possible to use Lwt_js_events.click here because the user
     may press the navigation buttons multiple times *)
  prev_btn##.onclick := Dom_html.handler (fun _ -> decr cur ; update () ; Js._false) ;
  next_btn##.onclick := Dom_html.handler (fun _ -> incr cur ; update () ; Js._false) ;

  let disable x =
    Ui_helpers.set_button_enabled "conf-prev" false ;
    Ui_helpers.set_button_enabled "conf-next" false ;
    Ui_helpers.set_button_enabled "conf-accept" false ;
    Ui_helpers.set_button_enabled "select-btn" true ;

    Lwt.return x
  in

  Lwt.pick [
    (Lwt_js_events.click accept_btn >>= disable >>= fun _ -> Lwt.return !cur) ;
    (Lwt_js_events.click load_btn >>= disable >>= fun _ -> Lwt.fail (Failure "Stop")) ;
    (Lwt_js_events.click stop_btn >>= disable >>= fun _ -> Lwt.fail (Failure "Stop")) ;
  ]

module RunMultiLts (MultiLts : Lts_kind.MULTI_RESULT_LTS_WITH_INIT) = struct
  include MultiLts

  module M = MyLwt

  let choose m =
    let open M in
    let res = EvalMonad.run m in
    let res_to_json = function
      | EvalMonad.PropStop -> None
      | EvalMonad.PropDiverges -> None
      | EvalMonad.Continue (_, pas_conf) ->
          let conf_json = passive_conf_to_yojson pas_conf in
          Some conf_json
    in
    let* choosen_conf = choose_conf (List.map res_to_json res) in
    return @@ List.nth res choosen_conf

  let _ = choose
end

module RunSingleLts (SingleLts : Lts_kind.SINGLE_RESULT_LTS_WITH_INIT) = struct
  include SingleLts

  module M = MyLwt

  let choose m = M.return (EvalMonad.run m)
end

let evaluate_code () =
  Moves_manager.flush_moves ();
  Editor_manager.fetch_editor_content ();

  let kind_lts = Lts_config.generate_kind_lts () in

  let lexBuffer_code = Lexing.from_string !Editor_manager.editor_content in
  let lexBuffer_sig = Lexing.from_string !Editor_manager.signature_content in

  Lexing.set_filename lexBuffer_code !Editor_manager.editor_filename;
  Lexing.set_filename lexBuffer_sig !Editor_manager.signature_filename;

  if kind_lts.Lts_kind.symbolic then
    let (module OGS_LTS) = Lts_kind.build_symbolic_lts kind_lts in
    let module RunLts = RunMultiLts (OGS_LTS) in
    let module IBuild = Lts.Interactive_build.Make (MyLwt) (RunLts) in

    let init_conf =
      OGS_LTS.Passive (OGS_LTS.lexing_init_pconf lexBuffer_code lexBuffer_sig)
    in

    match%lwt
        IBuild.interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
          init_conf
      with (* Should we deal with failure encapsulated in the Lwt monad ?*)
      | reason ->
          let () = Js.Unsafe.global##onSuccess (Js.Unsafe.inject reason) in
          Lwt.return 1
  else
    let (module OGS_LTS) = Lts_kind.build_concrete_lts kind_lts in
    let module RunLts = RunSingleLts (OGS_LTS) in
    let module IBuild = Lts.Interactive_build.Make (MyLwt) (RunLts) in

    let init_conf =
      OGS_LTS.Passive (OGS_LTS.lexing_init_pconf lexBuffer_code lexBuffer_sig)
    in

    match%lwt
        IBuild.interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
          init_conf
      with (* Should we deal with failure encapsulated in the Lwt monad ?*)
      | reason ->
          let () = Js.Unsafe.global##onSuccess (Js.Unsafe.inject reason) in
          Lwt.return 1
