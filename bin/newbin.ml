let number_filename = ref 0
let filename1 = ref ""
let filename2 = ref ""
let is_computation = ref false
let enable_wb = ref true
let enable_cps = ref true
let enable_visibility = ref true

let speclist =
  [
    ("-debug", Arg.Set Util.Debug.debug_mode, "Debug mode");
    ( "-comput",
      Arg.Set is_computation,
      "Provide a computation rather than a module." );
    ( "-no-wb",
      Arg.Clear enable_wb,
      "Disable the well-bracketing enforcment of the interaction." );
    ( "-no-vis",
      Arg.Clear enable_visibility,
      "Disable the visibility enforcment of the interaction" );
    ( "-no-cps",
      Arg.Clear enable_cps,
      "Use a representation of actions as calls and return rather than in cps \
       style." );
  ]

let usage_msg = "Usage: cavoctree filename [options]"

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
        ("Error: too many filenames have been provided. \n" ^ usage_msg)

let check_number_filenames i =
  if !number_filename != i then
    Util.Error.fail_error
      ("Error: a filenames containing the programs "
     ^ "should have been provided. " ^ usage_msg)

let generate (module OGS_LTS : Lts.Bipartite.INT_LTS) =
  let module Generate = Lts.Generate_trace.Make (OGS_LTS) in
  if !is_computation then begin
    check_number_filenames 1;
    Util.Debug.print_debug "Getting the program";
    let expr_buffer = open_in !filename1 in
    let (expr, namectxO) =
      OGS_LTS.Int.IntLang.get_typed_term "first" expr_buffer in
    Util.Debug.print_debug
      ("Name contexts for Opponent: "
      ^ OGS_LTS.Int.IntLang.string_of_name_ctx namectxO);
    let init_act_conf = OGS_LTS.init_aconf expr namectxO in
    let init_conf = OGS_LTS.Active init_act_conf in
    let traces = Generate.get_traces init_conf in
    List.iter print_endline traces
  end
  else begin
    check_number_filenames 2;
    Util.Debug.print_debug "Getting the module declaration";
    let decl_buffer = open_in !filename1 in
    let signature_buffer = open_in !filename2 in
    let (interactive_env, store, name_ctxP, name_ctxO) =
      OGS_LTS.Int.IntLang.get_typed_ienv decl_buffer signature_buffer in
    let init_pas_conf =
      OGS_LTS.init_pconf store interactive_env name_ctxP name_ctxO in
    let init_conf = OGS_LTS.Passive init_pas_conf in
    let traces = Generate.get_traces init_conf in
    List.iter print_endline traces
  end

let build_ogs_lts (module IntLang : Lang.Interactive.LANG) =
  let module Int = Lts.Interactive.Make (IntLang) in
  let module OGS_LTS = Ogs.Ogslts.Make (Int) in
  match (!enable_wb, !enable_visibility) with
  | (true, true) ->
      let module WBLTS = Ogs.Wblts.Make (Int.Name) (Int.Actions.Moves) in
      let module ProdLTS = Lts.Product_lts.Make (OGS_LTS) (WBLTS) in
      let module VisLTS = Ogs.Vis_lts.Make (Int.Name) (Int.Actions.Moves) in
      let module ProdLTS = Lts.Product_lts.Make (OGS_LTS) (VisLTS) in
      generate (module ProdLTS)
  | (true, false) ->
      let module WBLTS = Ogs.Wblts.Make (Int.Name) (Int.Actions.Moves) in
      let module ProdLTS = Lts.Product_lts.Make (OGS_LTS) (WBLTS) in
      generate (module ProdLTS)
  | (false, true) ->
      let module VisLTS = Ogs.Vis_lts.Make (Int.Name) (Int.Actions.Moves) in
      let module ProdLTS = Lts.Product_lts.Make (OGS_LTS) (VisLTS) in
      generate (module ProdLTS)
  | (false, false) -> generate (module OGS_LTS)

let () =
  Arg.parse speclist get_filename usage_msg;
  let module OpLang = Refml.RefML.WithAVal (Util.Monad.ListB) in
  let module CpsLang = Lang.Cps.MakeComp (OpLang) in
  if !enable_cps then
    let module CpsLang = Lang.Cps.MakeComp (OpLang) in
    let module IntLang = Lang.Interactive.Make (CpsLang) in
    build_ogs_lts (module IntLang)
  else
    let module DirectLang = Lang.Direct.Make (OpLang) in
    build_ogs_lts (module DirectLang)
