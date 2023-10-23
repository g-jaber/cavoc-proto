open Arg

let () =
  let number_filename = ref 0 in
  let filename = ref "" in
  let pogs = ref false in
  let ogs = ref false in
  let speclist =
    [("-debug",Set Cavoc.Debug.debug_mode,"Debug mode");
     ("-pogs", Set pogs, "Compute the Tree representation");
     ("-ogs", Set ogs, "Compute OGS (Beware, this might be infinite)")
    ] in
  let usage_msg = "Usage: cavoctree filename [options]" in
  let get_filename str =
    match !number_filename with
    | 0 -> filename := str; number_filename := !number_filename+1;
    | _ -> Cavoc.Error.fail_error ("Error: too many filenames have been provided. \n"^ usage_msg);
  in
  let check_number_filenames () =
    if !number_filename = 0 then
      Cavoc.Error.fail_error ("Error: a filenames containing the programs "
        ^ "should have been provided. "^ usage_msg);
  in
  parse speclist get_filename usage_msg;
  check_number_filenames ();
  let inBuffer = open_in !filename in
  let (expr,namectxO) = Cavoc.RefML.RefML.get_typed_computation "first" inBuffer in
  let module POGS = Cavoc.Pogs.POGS(Cavoc.RefML.RefML)(Cavoc.Monad.ListMonad) in
  let init_aconf = POGS.init_aconf expr namectxO in
  let module POGS_LTS = Cavoc.Graph.Graph(Cavoc.RefML.RefML)(Cavoc.Monad.ListMonad)(Cavoc.Pogs.POGS) in
  let pogs = POGS_LTS.compute_graph init_aconf in
  let lts_string = POGS_LTS.string_of_graph pogs in
  print_string lts_string;;