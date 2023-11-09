let () =
  let number_filename = ref 0 in
  let filename1 = ref "" in
  let filename2 = ref "" in
  let is_computation = ref false in
  let speclist =
    [
      ("-debug", Arg.Set Util.Debug.debug_mode, "Debug mode");
      ( "-comput",
        Arg.Set is_computation,
        "Provide a computation rather than a module" );
    ] in
  let usage_msg = "Usage: cavoctree filename [options]" in
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
  let check_number_filenames i =
    if !number_filename != i then
      Util.Error.fail_error
        ("Error: a filenames containing the programs "
       ^ "should have been provided. " ^ usage_msg) in
  Arg.parse speclist get_filename usage_msg;
  let module Int = Cavoc.Cps.Int_Make (Refml.RefML.RefML) in
  let module OGS_LTS = Cavoc.Ogs.OgsLtsF (Util.Monad.ListB) (Int) in
  let module WBLTS = Cavoc.Wblts.WBLTS (Int.ContNames) (Int.Actions.Moves) in
  let module ProdLTS = Cavoc.Product_lts.Make (OGS_LTS) (WBLTS) in
  let init_conf =
    if !is_computation then begin
      check_number_filenames 1;
      Util.Debug.print_debug "Getting the program";
      let expr_buffer = open_in !filename1 in
      let (expr, namectxO) =
        Int.OpLang.get_typed_computation "first" expr_buffer in
      Util.Debug.print_debug
        ("Name contexts for Opponent: "
        ^ Int.OpLang.string_of_name_type_ctx namectxO);
      let init_act_conf = ProdLTS.init_aconf expr namectxO in
      ProdLTS.Active init_act_conf
    end
    else begin
      check_number_filenames 2;
      Util.Debug.print_debug "Getting the module declaration";
      let decl_buffer = open_in !filename1 in
      let signature_buffer = open_in !filename2 in
      let (interactive_env, resources, name_type_ctxP, name_type_ctxO) =
        Int.OpLang.get_typed_ienv decl_buffer signature_buffer in
      let init_pas_conf =
        ProdLTS.init_pconf resources interactive_env name_type_ctxP
          name_type_ctxO in
      ProdLTS.Passive init_pas_conf
    end in
  Util.Debug.print_debug "Getting the trace";
  let module Generate = Cavoc.Generate_trace.Make (ProdLTS) in
  let traces = Generate.get_traces init_conf in
  List.iter print_endline traces
