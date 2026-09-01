(* ==================================================
   EDITOR_MANAGER: the per-participant editor registry
   ==================================================
   Reads the sources of the participant cards through window.cavocCards,
   the registry that front/common.js (cavocBuildCards) maintains: one entry
   per card, holding the participant's name and its two ACE editors.
*)

open Js_of_ocaml

type participant_source = {
  participant_name: string;
      (** the file name stem the lexer reports in error messages *)
  module_code: string;
  signature_code: string;
}

let editor_value editor : string =
  Js.to_string (Js.Unsafe.meth_call editor "getValue" [||])

(* The cards left to right, i.e. in the order of the scenario's participants
   list; empty when the page has no cards. *)
let fetch_participant_sources () : participant_source list =
  let cards : 'a Js.Optdef.t =
    Js.Unsafe.get Js.Unsafe.global (Js.string "cavocCards") in
  Js.Optdef.case cards
    (fun () -> [])
    (fun cards ->
      let count : int = Js.Unsafe.get cards (Js.string "length") in
      List.init count (fun index ->
          let card = Js.Unsafe.get cards index in
          {
            participant_name= Js.to_string (Js.Unsafe.get card "name");
            module_code= editor_value (Js.Unsafe.get card "editor");
            signature_code=
              editor_value (Js.Unsafe.get card "signatureEditor");
          }))
