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

(* Reports why the interaction stopped. Leaving the game is the user's own
   doing, so it is only logged; the other outcomes are worth a modal. *)
let report_interaction_end (outcome : Lts.Interactive_build.outcome) =
  let open Lts.Interactive_build in
  match outcome with
  | User_quit ->
      Ui_helpers.print_to_output (string_of_outcome outcome);
      Lwt.return_unit
  | Prop_stopped | Prop_diverges ->
      let message = Js.string (string_of_outcome outcome) in
      let () = Js.Unsafe.global##onSuccess (Js.Unsafe.inject message) in
      Lwt.return_unit

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
  let open Interaction_signals in
  let nb_moves = n + 1 in
  let%lwt choice = Moves_display.get_chosen_move () in
  match choice with
  | Chosen i when i >= 0 && i < nb_moves ->
      Ui_helpers.print_to_output ("Selected move: " ^ string_of_int i);
      Lwt.return (Lts.Interactive_build.Chose i)
  | Chosen i ->
      Ui_helpers.print_to_output
        (Printf.sprintf "Move index %d out of range (0..%d)" i (nb_moves - 1));
      Lwt.fail (Failure "Move index out of range")
  | Interrupted ->
      Moves_manager.clear_list ();
      Lwt.return Lts.Interactive_build.Quit
  | (No_selection | Missing_buttons) as choice ->
      let msg = string_of_move_choice choice in
      Ui_helpers.print_to_output ("Error: " ^ msg);
      Lwt.fail (Failure msg)

(* A click on one of these buttons abandons the current interaction. They are
   optional: a page need not offer every way of leaving the game. *)
let interrupt_buttons () =
  List.filter_map Dom_html.getElementById_opt [ "load-btn"; "stop-btn" ]

(* What to show for one of the configurations the user is browsing. Terminal
   branches carry their own explanation, so that Proponent stopping and the
   program diverging stay distinguishable. *)
type conf_preview =
  | Runnable of Yojson.Safe.t  (** the passive configuration to display *)
  | Terminal of string  (** why this branch has no continuation *)

let choose_conf confs =
  let open Lts.Interactive_build in
  let nconf = List.length confs in

  (* avoid prompting the user everytime the LTS stops *)
  if nconf = 1 then Lwt.return (Chose 0) else

  match
    ( Dom_html.getElementById_opt "conf-accept",
      Dom_html.getElementById_opt "conf-prev",
      Dom_html.getElementById_opt "conf-next" )
  with
  | None, _, _ | _, None, _ | _, _, None ->
      (* This page offers no way to navigate between configurations, so waiting
         for a click that cannot happen would hang the interaction. *)
      Ui_helpers.print_to_output
        (Printf.sprintf
           "No configuration navigation on this page: selecting the first of \
            %d configurations."
           nconf);
      Lwt.return (Chose 0)
  | Some accept_btn, Some prev_btn, Some next_btn ->

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
    | Runnable conf -> show_conf conf
    | Terminal reason -> Display_config.display_terminal_conf reason
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

  Lwt.pick
    ((Lwt_js_events.click accept_btn >>= disable >>= fun _ ->
      Lwt.return (Chose !cur))
     :: List.map
          (fun btn ->
            Lwt_js_events.click btn >>= disable >>= fun _ -> Lwt.return Quit)
          (interrupt_buttons ()))

module RunMultiLts (MultiLts : Lts_kind.MULTI_RESULT_LTS_WITH_INIT) = struct
  include MultiLts

  module M = MyLwt

  let choose m =
    let open M in
    let open Lts.Interactive_build in
    let res = EvalMonad.run m in
    let res_to_preview = function
      | EvalMonad.PropStop ->
          Terminal "Proponent stops playing in this configuration."
      | EvalMonad.PropDiverges ->
          Terminal "The program diverges in this configuration."
      | EvalMonad.Continue (_, pas_conf) ->
          Runnable (passive_conf_to_yojson pas_conf)
    in
    let* choosen_conf = choose_conf (List.map res_to_preview res) in
    match choosen_conf with
    | Quit -> return Quit
    | Chose i -> return (Chose (List.nth res i))

  let _ = choose
end

module RunSingleLts (SingleLts : Lts_kind.SINGLE_RESULT_LTS_WITH_INIT) = struct
  include SingleLts

  module M = MyLwt

  let choose m =
    let open Lts.Interactive_build in
    M.return (Chose (EvalMonad.run m))
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

    let%lwt outcome =
      IBuild.interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
        init_conf in
    report_interaction_end outcome
  else
    let (module OGS_LTS) = Lts_kind.build_concrete_lts kind_lts in
    let module RunLts = RunSingleLts (OGS_LTS) in
    let module IBuild = Lts.Interactive_build.Make (MyLwt) (RunLts) in

    let init_conf =
      OGS_LTS.Passive (OGS_LTS.lexing_init_pconf lexBuffer_code lexBuffer_sig)
    in

    let%lwt outcome =
      IBuild.interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
        init_conf in
    report_interaction_end outcome
