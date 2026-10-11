(* =============================================
   SYNTHESIS_DISPLAY: the synthesized programs
   =============================================
   Shows the programs the interaction defines: the Client tab of the
   single-module page, and the two read-only cards of the synthesis page. *)

open Js_of_ocaml

let waiting_line = "The client appears here once you have played a move."

let parse_refml_source source =
  let parse entry = entry Refml.Lexer.token (Lexing.from_string source) in
  try Some (`Declarations (parse Refml.Parser.prog)) with
  | Refml.Parser.Error | Refml.Lexer.SyntaxError _ -> (
      try Some (`Expression (parse Refml.Parser.fullexpr))
      with Refml.Parser.Error | Refml.Lexer.SyntaxError _ -> None)

(* The ocamlformat bundle (bin/ocamlformat_web) loads on its own, so it may be missing. *)
let format_with_ocamlformat source =
  match Js.Optdef.to_option (Js.Unsafe.get Js.Unsafe.global "cavocFormatOcaml") with
  | None -> None
  | Some format_ocaml -> (
      let formatted : Js.js_string Js.t Js.opt =
        Js.Unsafe.fun_call format_ocaml [| Js.Unsafe.inject (Js.string source) |]
      in
      match Js.Opt.to_option formatted with
      | None -> None
      | Some formatted -> Some (Js.to_string formatted))

(* The source is kept unformatted unless RefML reads the formatted one identically. *)
let format_synthesized_source source =
  match format_with_ocamlformat source with
  | None -> source
  | Some formatted -> (
      match (parse_refml_source source, parse_refml_source formatted) with
      | (Some ast, Some formatted_ast) when ast = formatted_ast -> formatted
      | _ -> source)

(* The Client tab is a read-only ACE editor (front/common.js). *)
let set_client_code text : unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.get Js.Unsafe.global "cavocSetClientCode")
    [| Js.Unsafe.inject (Js.string text) |]

let reset () = set_client_code ("(* " ^ waiting_line ^ " *)")
let display_no_client reason = set_client_code ("(* " ^ reason ^ " *)")

(* The synthesis fails outside the definable fragment, so it is run
   here, where its reason can be shown in place of the code. *)
let display_synthesized_client synthesize =
  match synthesize () with
  | None -> reset ()
  | Some source -> set_client_code (format_synthesized_source source)
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
    | Some source -> format_synthesized_source source
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
