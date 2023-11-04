open Arg

let () =
let number_filename = ref 0 in
let filename = ref "" in
let speclist =
  [("-debug",Set Util.Debug.debug_mode,"Debug mode");
  ] in
let usage_msg = "Usage: cavoctree filename [options]" in
let get_filename str =
  match !number_filename with
  | 0 -> filename := str; number_filename := !number_filename+1;
  | _ -> Util.Error.fail_error ("Error: too many filenames have been provided. \n"^ usage_msg);
in
let check_number_filenames () =
  if !number_filename = 0 then
    Util.Error.fail_error ("Error: a filenames containing the programs "
      ^ "should have been provided. "^ usage_msg);
in
parse speclist get_filename usage_msg;
check_number_filenames ();
  let module Int = Cavoc.Cps.Int_Make(Refml.RefML.RefML) in
  let module OGS_LTS = Cavoc.Ogs.OgsLtsF(Util.Monad.ListB)(Int) in
  let module WBLTS = Cavoc.Wblts.WBLTS(Int.ContNames)(Int.Actions.Moves) in
  let module ProdLTS = Cavoc.Product_lts.Make(OGS_LTS)(WBLTS) in
  Util.Debug.print_debug "Getting the program";
  let inBuffer1 = open_in !filename in
  let (expr,namectxO) = Int.OpLang.get_typed_computation "first" inBuffer1 in
  Util.Debug.print_debug 
    ("Name contexts for Opponent: " ^ (Int.OpLang.string_of_name_type_ctx namectxO));
  let init_aconf = ProdLTS.init_aconf expr namectxO in
  Util.Debug.print_debug "Getting the trace";
  let module Generate = Cavoc.Generate_trace.Make(ProdLTS) in
  let traces = Generate.get_traces init_aconf in
  List.iter print_endline traces;;