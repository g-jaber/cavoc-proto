open Arg

let () =
  let number_filename = ref 0 in
  let filename1 = ref "" in
  let filename2 = ref "" in
  let filename3 = ref "" in
  let speclist = [ ("-debug", Set Util.Debug.debug_mode, "Debug mode") ] in
  let usage_msg = "Usage: compose prog.ml module.ml module.mli" in
  let get_filename str =
    match !number_filename with
    | 0 ->
        filename1 := str;
        number_filename := !number_filename + 1
    | 1 ->
        filename2 := str;
        number_filename := !number_filename + 1
    | 2 ->
        filename3 := str;
        number_filename := !number_filename + 1
    | _ ->
        Util.Error.fail_error
          ("Error: too many filenames have been provided. \n" ^ usage_msg) in
  let check_number_filenames () =
    if !number_filename < 3 then
      Util.Error.fail_error
        ("Error: three filenames containing the programs to be compared "
       ^ "should have been provided. " ^ usage_msg) in
  parse speclist get_filename usage_msg;
  check_number_filenames ();
  let module OpLang = Refml.RefML.WithAVal (Util.Monad.ListB) in
  let module CpsLang = Lang.Cps.MakeComp (OpLang) () in
  let module IntLang = Lang.Interactive.Make (CpsLang) in
  let module TypingLTS = Ogs.Typing.Make (IntLang) in
  let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
  let module ProdLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
  let module OGS_LTS = Ogs.Ogslts.Make (IntLang) (ProdLTS) in
  Util.Debug.print_debug "Getting the program";
  let inBuffer1 = open_in !filename1 in
  let lexBuffer1 = Lexing.from_channel inBuffer1 in
  let (opconf, namectxO) = IntLang.get_typed_opconf "first" lexBuffer1 in
  Util.Debug.print_debug "Getting the module";
  let inBuffer2 = open_in !filename2 in
  let inBuffer3 = open_in !filename3 in
  let lexBuffer2 = Lexing.from_channel inBuffer2 in
  let lexBuffer3 = Lexing.from_channel inBuffer3 in
  let (ienv, store, namectxO', _) =
    IntLang.get_typed_ienv lexBuffer2 lexBuffer3 in
  Util.Debug.print_debug
    ("Name contexts for Opponent: "
    ^ IntLang.Namectx.to_string namectxO
    ^ " and "
    ^ IntLang.Namectx.to_string namectxO');
  let init_aconf =
    OGS_LTS.Active
      (* The following concatenation should be reworked *)
      (OGS_LTS.init_aconf opconf (IntLang.Namectx.concat namectxO namectxO'))
  in
  let init_pconf =
    OGS_LTS.Passive
      (OGS_LTS.init_pconf store ienv namectxO' IntLang.Namectx.empty) in
  let module Composed_LTS = Lts.Compose.Make (OGS_LTS) in
  let traces = Composed_LTS.get_traces_check init_aconf init_pconf in
  Util.Debug.print_debug "Getting the trace";
  List.iter print_endline traces
