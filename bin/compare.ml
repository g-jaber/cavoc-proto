open Arg

let () =
  let number_filename = ref 0 in
  let filename1 = ref "" in
  let filename2 = ref "" in
  let speclist = [ ("-debug", Set Util.Debug.debug_mode, "Debug mode") ] in
  let usage_msg = "Usage: compare filename1 filename2 [options]" in
  let get_filename str =
    match !number_filename with
    | 0 ->
        filename1 := str;
        number_filename := !number_filename + 1
    | 1 ->
        filename2 := str;
        number_filename := !number_filename + 1
    | _ ->
        Util.Error.fail_error
          ("Error: too many filenames have been provided. \n" ^ usage_msg) in
  let check_number_filenames () =
    if !number_filename < 2 then
      Util.Error.fail_error
        ("Error: two filenames containing the programs to be compared "
       ^ "should have been provided. " ^ usage_msg) in
  parse speclist get_filename usage_msg;
  check_number_filenames ();
  let module OpLang = Refml.RefML.WithAVal (Util.Monad.ListB) in
  let module CpsLang = Lang.Cps.MakeComp (OpLang) in
  let module IntLang = Lang.Interactive.Make (CpsLang) in
  let module Int = Lts.Interactive.Make (IntLang) in
  let module POGS_LTS = Pogs.Pogslts.Make (Int) in
  Util.Debug.print_debug "Getting the first program";
  let inBuffer1 = open_in !filename1 in
  let (expr1, namectxO1) = Int.IntLang.get_typed_term "first" inBuffer1 in
  Util.Debug.print_debug "Getting the second program";
  let inBuffer2 = open_in !filename2 in
  let (expr2, namectxO2) = Int.IntLang.get_typed_term "second" inBuffer2 in
  Util.Debug.print_debug
    ("Name contexts for Opponent: "
    ^ Int.IntLang.string_of_name_ctx namectxO1
    ^ " and "
    ^ Int.IntLang.string_of_name_ctx namectxO2);
  let module Synch_LTS = Lts.Synch_lts.Make (POGS_LTS) in
  let init_aconf = Synch_LTS.init_aconf expr1 namectxO1 expr2 namectxO2 in
  let module Graph = Lts.Graph.Graph (Synch_LTS) in
  let graph = Graph.compute_graph init_aconf in
  let graph_string = Graph.string_of_graph graph in
  print_string graph_string
