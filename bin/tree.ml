open Arg

let () =
  let number_filename = ref 0 in
  let filename = ref "" in
  let speclist = [ ("-debug", Set Util.Debug.debug_mode, "Debug mode") ] in
  let usage_msg = "Usage: tree filename [options]" in
  let get_filename str =
    match !number_filename with
    | 0 ->
        filename := str;
        number_filename := !number_filename + 1
    | _ ->
        Util.Error.fail_error
          ("Error: too many filenames have been provided. \n" ^ usage_msg) in
  let check_number_filenames () =
    if !number_filename = 0 then
      Util.Error.fail_error
        ("Error: a filenames containing the programs "
       ^ "should have been provided. " ^ usage_msg) in
  parse speclist get_filename usage_msg;
  check_number_filenames ();
  let inBuffer = open_in !filename in
  let module OpLang = Refml.RefML.WithAVal (Util.Monad.ListB) in
  let module CpsLang = Lang.Cps.MakeComp (OpLang) in
  let module IntLang = Lang.Interactive.Make (CpsLang) in
  let module Int = Lts.Interactive.Make (IntLang) in
  let (expr, namectxO) = Int.IntLang.get_typed_term "first" inBuffer in
  let module POGS_LTS = Pogs.Pogslts.Make (Int) in
  let init_aconf = POGS_LTS.init_aconf expr namectxO in
  let module Graph = Lts.Graph.Graph (POGS_LTS) in
  let graph = Graph.compute_graph init_aconf in
  let graph_string = Graph.string_of_graph graph in
  print_string graph_string
