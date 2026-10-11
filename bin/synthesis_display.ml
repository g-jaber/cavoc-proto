(* =============================================
   SYNTHESIS_DISPLAY: the synthesized programs
   =============================================
   Shows the programs the interaction defines: the Client tab of the
   single-module page, and the two read-only cards of the synthesis page. *)

open Js_of_ocaml

let waiting_line = "The client appears here once you have played a move."

(* The Client tab is a read-only ACE editor (front/common.js). *)
let set_client_code text : unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.get Js.Unsafe.global "cavocSetClientCode")
    [| Js.Unsafe.inject (Js.string text) |]

let reset () = set_client_code ("(* " ^ waiting_line ^ " *)")
let display_no_client reason = set_client_code ("(* " ^ reason ^ " *)")

(* The synthesis fails outside the definable fragment, so it is run
   here, where its reason can be shown in place of the code. *)
let display_synthesized_client ~imports synthesize =
  match synthesize () with
  | None -> reset ()
  | Some source ->
      let header =
        match String.trim imports with
        | "" -> ""
        | imports -> "(* imports *)\n" ^ imports ^ "\n\n(* client *)\n" in
      set_client_code (header ^ source)
  | exception Failure reason -> display_no_client reason

(* The read-only module pane of a card (front/common.js). *)
let set_card_code card text : unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.get Js.Unsafe.global "cavocSetCardCode")
    [| Js.Unsafe.inject card; Js.Unsafe.inject (Js.string text) |]

(* A card holds source, so what is not source is shown as a comment. *)
let display_in_card card ~waiting synthesize =
  set_card_code card
    (match synthesize () with
    | Some source -> source
    | None -> "(* " ^ waiting ^ " *)"
    | exception Failure reason -> "(* " ^ reason ^ " *)")

let module_card = 0
let client_card = 1

let display_synthesized_participants ~module_source ~client_source =
  display_in_card module_card
    ~waiting:"the module appears once the client has been answered"
    module_source;
  display_in_card client_card
    ~waiting:"the client appears once a move has been played" client_source

let reset_participants () =
  display_synthesized_participants
    ~module_source:(fun () -> None)
    ~client_source:(fun () -> None)
