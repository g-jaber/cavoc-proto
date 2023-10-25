open Arg

let () =
  let number_filename = ref 0 in
  let filename1 = ref "" in
  let filename2 = ref "" in
  let pogs = ref false in
  let ogs = ref false in
  let speclist =
    [("-debug",Set Cavoc.Debug.debug_mode,"Debug mode");
     ("-pogs", Set pogs, "Compute the Tree representation");
     ("-ogs", Set ogs, "Compute BILTS (Beware, this might be infinite)");
    ] in
  let usage_msg = "Usage: cavocsynch filename1 filename2 [options]" in
  let get_filename str =
    match !number_filename with
    | 0 -> filename1 := str; number_filename := !number_filename+1;
    | 1 -> filename2 := str; number_filename := !number_filename+1;
    | _ -> Cavoc.Error.fail_error  ("Error: too many filenames have been provided. \n"^ usage_msg);
  in
  let check_number_filenames () =
    if !number_filename < 2 then
      Cavoc.Error.fail_error  ("Error: two filenames containing the programs to be compared "
        ^ "should have been provided. "^ usage_msg);
  in
  parse speclist get_filename usage_msg;
  check_number_filenames ();
  (*let module Moves = Cavoc.Cps.Moves(Cavoc.RefML.RefML) in*)
  let module Int = Cavoc.Cps.Int_Make(Cavoc.RefML.RefML) in
  let module OGS_LTS = Cavoc.Ogs.OgsLtsF(Cavoc.Monad.ListMonad)(Int) in
  let module WBLTS = Cavoc.Wblts.WBLTS(Int.Moves) in
  let module ProdLTS = Cavoc.Product_lts.Make(OGS_LTS)(WBLTS) in
  Cavoc.Debug.print_debug "Getting the program";
  let inBuffer1 = open_in !filename1 in
  let (expr1,namectxO) = Int.Actions.Lang.get_typed_computation "first" inBuffer1 in
  Cavoc.Debug.print_debug "Getting the module";
  let inBuffer2 = open_in !filename2 in
  let (ienv,namectxO') = Int.Actions.Lang.get_typed_ienv inBuffer2 in
  Cavoc.Debug.print_debug 
    ("Name contexts for Opponent: " ^ (Int.Actions.Lang.string_of_name_type_ctx namectxO) 
    ^ " and " ^ (Int.Actions.Lang.string_of_name_type_ctx namectxO'));
  let init_aconf = ProdLTS.Active (ProdLTS.init_aconf expr1 (Cavoc.Pmap.concat namectxO namectxO')) in
  let init_pconf = ProdLTS.Passive (ProdLTS.init_pconf ienv namectxO' Cavoc.Pmap.empty) in
  let module Synchronized_LTS = Cavoc.Synchronize.Make(ProdLTS) in
  let namespan = Cavoc.Namespan.id_nspan (Cavoc.Pmap.dom namectxO') in
  let traces = Synchronized_LTS.get_traces namespan init_aconf init_pconf in
  Cavoc.Debug.print_debug "Getting the trace";
  List.iter print_endline traces;;