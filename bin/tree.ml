open Arg

let () =
  let number_filename = ref 0 in
  let filename = ref "" in
  let speclist = [ ("-debug", Set Util.Debug.debug_mode, "Debug mode") ] in
  let usage_msg = "Usage: cavoctree filename [options]" in
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
  let module Int =Lts.Cps.Int_Make (Refml.RefML.RefML) in
  let (expr, namectxO) = Int.IntLang.get_typed_computation "first" inBuffer in
  let module POGS_LTS = Pogs.Pogslts.Make (Util.Monad.ListB) (Int) in
  let init_aconf = POGS_LTS.init_aconf expr namectxO in
  let module Graph = Lts.Graph.Graph (POGS_LTS) in
  let graph = Graph.compute_graph init_aconf in
  let graph_string = Graph.string_of_graph graph in
  print_string graph_string
