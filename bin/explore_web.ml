open Js_of_ocaml

type mode = Explore
(* Only Explore mode is needed *)

let is_mode = ref Explore
let print_dot = ref false

(* Fetches the content from the HTML editor fields *)
let editor_content = ref ""
let signature_content = ref ""

let fetch_editor_content () =
  (* Access the editor instance directly from the global window object *)
  let editor = Js.Unsafe.get Js.Unsafe.global "editor_instance" in
  let signature_editor =
    Js.Unsafe.get Js.Unsafe.global "signatureEditor_instance" in

  (* Call getValue() on the ACE editor instances *)
  editor_content := Js.to_string (Js.Unsafe.meth_call editor "getValue" [||]);
  signature_content :=
    Js.to_string (Js.Unsafe.meth_call signature_editor "getValue" [||])

(* Redirects OCaml print functions to output in the HTML div with id "output" *)
let print_to_output str =
  let output_div = Dom_html.getElementById "output" in
  let current_content = Js.to_string (Js.Unsafe.get output_div "innerHTML") in
  let new_content = current_content ^ "<br>" ^ str in
  Js.Unsafe.set output_div "innerHTML" (Js.string new_content)

(* Overrides default print functions to redirect to the HTML output div *)
let () =
  Printexc.record_backtrace true;
  Sys_js.set_channel_flusher stdout print_to_output;
  Sys_js.set_channel_flusher stderr print_to_output

(* Build graph function remains the same as it visualizes the LTS in a graph format *)
let build_graph (type a) (module Graph : Lts.Graph.GRAPH with type conf = a)
    (init_conf : a) =
  let graph = Graph.compute_graph init_conf in
  let graph_string = Graph.string_of_graph graph in
  print_string graph_string

(* Generates the LTS based on the selected mode and editor content *)
let generate (module OGS_LTS : Lts.Bipartite.INT_LTS) =
  match !is_mode with
  | Explore ->
      Util.Debug.print_debug "Getting the program from the editor";
      ignore (print_to_output "Trying to get code, code is :");
      ignore (print_to_output ("Code :" ^ !editor_content));
      let lexBuffer = Lexing.from_string !editor_content in
      let (expr, namectxO) =
        OGS_LTS.Int.IntLang.get_typed_term "first" lexBuffer in
      Util.Debug.print_debug
        ("Name contexts for Opponent: "
        ^ OGS_LTS.Int.IntLang.string_of_name_ctx namectxO);
      let init_conf = OGS_LTS.Active (OGS_LTS.init_aconf expr namectxO) in
      if !print_dot then
        let module Graph = Lts.Graph.Make (OGS_LTS) in
        build_graph (module Graph) init_conf
      else
        let module Generate = Lts.Generate_trace.Make (OGS_LTS) in
        build_graph (module Generate) init_conf

(* Builds and evaluates the OGS LTS based on the provided code content *)
let evaluate_code () =
  (* Fetch editor content and store in refs *)
  fetch_editor_content ();

  let module OpLang = Refml.RefML.WithAVal (Util.Monad.ListB) in
  let module DirectLang = Lang.Direct.Make (OpLang) in
  let module Int = Lts.Interactive.Make (DirectLang) in
  let module POGS_LTS = Pogs.Pogslts.Make (Int) in
  generate (module POGS_LTS);

  (* Set options based on flags *)
  let module OpLang = Refml.RefML.WithAVal (Util.Monad.ListB) in
  let module CpsLang = Lang.Cps.MakeComp (OpLang) in
  let module IntLang = Lang.Interactive.Make (CpsLang) in
  let module Int = Lts.Interactive.Make (IntLang) in
  let module OGS_LTS = Ogs.Ogslts.Make (Int) in
  let lexBuffer_code = Lexing.from_string !editor_content in
  let lexBuffer_sig = Lexing.from_string !signature_content in
  let (interactive_env, store, name_ctxP, name_ctxO) =
    OGS_LTS.Int.IntLang.get_typed_ienv lexBuffer_code lexBuffer_sig in
  let init_conf =
    OGS_LTS.Passive
      (OGS_LTS.init_pconf store interactive_env name_ctxP name_ctxO) in
  let module Generate = Lts.Generate_trace.Make (OGS_LTS) in
  build_graph (module Generate) init_conf

(* Sets up the event listener for the "Evaluer" button *)
let () =
  let button = Dom_html.getElementById "submit" in
  ignore
    (Dom_html.addEventListener button Dom_html.Event.click
       (Dom_html.handler (fun _ ->
            evaluate_code ();
            Js._false))
       Js._true)
