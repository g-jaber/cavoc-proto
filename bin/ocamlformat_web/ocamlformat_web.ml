(* =============================================
   OCAMLFORMAT_WEB: ocamlformat for the web page
   =============================================
   A bundle of its own, since ocp-indent's Util module clashes with CAVOC's. *)

open Js_of_ocaml

(* RefML requires the parentheses of tuple patterns. *)
let ocamlformat_conf =
  match
    Ocamlformat_lib.Conf.update_value Ocamlformat_lib.Conf.default
      ~name:"parens-tuple-patterns" ~value:"always"
  with
  | Ok conf -> conf
  | Error _ -> Ocamlformat_lib.Conf.default

let format_ocaml source =
  match
    Ocamlformat_lib.Translation_unit.parse_and_format
      Ocamlformat_lib.Syntax.Use_file ~input_name:"synthesized.ml"
      ~source:(Js.to_string source) ocamlformat_conf
  with
  | Ok formatted -> Js.some (Js.string formatted)
  | Error _ -> Js.null

let () = Js.export "cavocFormatOcaml" (Js.wrap_callback format_ocaml)
