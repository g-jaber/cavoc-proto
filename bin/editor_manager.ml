(* ============================================
   EDITOR_MANAGER: ACE editor instance management
   ============================================
   Manages references to code and signature editor instances:
   - editor_content: Current code in main editor
   - signature_content: Current signature definition
   - fetch_editor_content: Retrieves content from both editors
*)

open Js_of_ocaml

let editor_content = ref ""
let signature_content = ref ""

let fetch_editor_content () =
  let editor = Js.Unsafe.get Js.Unsafe.global "editor_instance" in
  let signature_editor =
    Js.Unsafe.get Js.Unsafe.global "signatureEditor_instance" in

  editor_content := Js.to_string (Js.Unsafe.meth_call editor "getValue" [||]);
  signature_content :=
    Js.to_string (Js.Unsafe.meth_call signature_editor "getValue" [||])
