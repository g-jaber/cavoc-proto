let () =
  let number_filename = ref 0 in
  let filename1 = ref "" in
  let filename2 = ref "" in
  let is_computation = ref false in
  let enable_wb = ref true in
  let speclist =
    [
      ("-debug", Arg.Set Util.Debug.debug_mode, "Debug mode");
      ( "-comput",
        Arg.Set is_computation,
        "Provide a computation rather than a module." );
      ( "-no-wb",
        Arg.Clear enable_wb,
        "Disable the well-bracketing enforcment of well-bracketing." );
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
  let module OpLang = Refml.RefML.WithAVal (Util.Monad.ListB) in
  let module CpsLang = Lang.Cps.MakeComp (OpLang) in
  let module IntLang = Lang.Direct.Make (OpLang) in
  let module IntLang = Lang.Interactive.Make (CpsLang) in
  let module Int = Lts.Interactive.Make (IntLang) in
  let module OGS_LTS = Ogs.Ogslts.Make (Int) in
  let module WBLTS = Ogs.Wblts.Make (Int.Name) (Int.Actions.Moves) in
  let module ProdLTS = Lts.Product_lts.Make (OGS_LTS) (WBLTS) in
  Util.Debug.print_debug "Getting the trace";
  if !is_computation then begin
    check_number_filenames 1;
    Util.Debug.print_debug "Getting the program";
    let expr_buffer = open_in !filename1 in
    let (expr, namectxO) = Int.IntLang.get_typed_term "first" expr_buffer in
    Util.Debug.print_debug
      ("Name contexts for Opponent: " ^ Int.IntLang.string_of_name_ctx namectxO);
    let init_act_conf = ProdLTS.init_aconf expr namectxO in
    let init_conf = ProdLTS.Active init_act_conf in
    let module Generate = Lts.Generate_trace.Make (ProdLTS) in
    let graph = Generate.compute_graph init_conf in
    let graph_string = Generate.string_of_graph graph in
    print_string graph_string
  end
  else begin
    check_number_filenames 2;
    Util.Debug.print_debug "Getting the module declaration";
    let decl_buffer = open_in !filename1 in
    let signature_buffer = open_in !filename2 in
    let (interactive_env, store, name_ctxP, name_ctxO) =
      Int.IntLang.get_typed_ienv decl_buffer signature_buffer in
    if !enable_wb then
      let init_pas_conf =
        ProdLTS.init_pconf store interactive_env name_ctxP name_ctxO in
      let init_conf = ProdLTS.Passive init_pas_conf in
      let module Generate = Lts.Generate_trace.Make (ProdLTS) in
      let graph = Generate.compute_graph init_conf in
      let graph_string = Generate.string_of_graph graph in
      print_string graph_string
    else
      let init_pas_conf =
        OGS_LTS.init_pconf store interactive_env name_ctxP name_ctxO in
      let init_conf = OGS_LTS.Passive init_pas_conf in
      let module Generate = Lts.Generate_trace.Make (OGS_LTS) in
      let graph = Generate.compute_graph init_conf in
      let graph_string = Generate.string_of_graph graph in
      print_string graph_string
  end
