type mode =
  | Explore
  (* id 0 *)
  | Compare
  (* id 1 *)
  | Compose (* id 2 *)

(*
type kind_exe =
  | Prog of string
  | Module of (string*string)
*)
let number_filename = ref 0
let filename1 = ref ""
let filename2 = ref ""
let filename3 = ref ""
let is_program = ref false
let enable_wb = ref true
let enable_cps = ref true
let enable_visibility = ref false
let generate_tree = ref false
let print_dot = ref false
let is_mode = ref Explore
let is_compare = ref false
let is_compose = ref false

let speclist =
  [
    ("-debug", Arg.Set Util.Debug.debug_mode, "Debug mode");
    ("-generate-tree", Arg.Set generate_tree, "Generate the normal-form tree");
    ("-compare", Arg.Set is_compare, "Compare the two modules or programs");
    ("-program", Arg.Set is_program, "Provide a program rather than a module.");
    ( "-no-wb",
      Arg.Clear enable_wb,
      "Disable the well-bracketing enforcement of the interaction." );
    ( "-vis",
      Arg.Set enable_visibility,
      "Enable the visibility enforcement of the interaction" );
    ( "-no-cps",
      Tuple [Arg.Clear enable_cps; Arg.Clear enable_wb],
      "Use a representation of actions as calls and return rather than in cps \
       style. This is incompatible with both visibility restriction and open \
       composition." );
  ]

let usage_msg = "Usage: explore filename.ml filename.mli [options]"

let fix_mode () =
  match (!is_compare, !is_compose) with
  | (false, false) -> is_mode := Explore
  | (true, false) -> is_mode := Compare
  | (false, true) -> is_mode := Compose
  | (true, true) ->
      failwith
        "Error: the mode Compare and Compose are both set to true. Please \
         report"

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
        ("Error: too many filenames have been provided. \n" ^ usage_msg)

let get_number_filename () =
  match (!is_mode, !is_program) with
  | (Explore, false) -> 2
  | (Explore, true) -> 1 (* no mli *)
  | (Compare, false) -> 3 (* one common mli for both modules *)
  | (Compare, true) -> 2 (* no mli *)
  | (Compose, false) -> 3 (* one common mli for both modules *)
  | (Compose, true) -> 2 (* no mli *)

let check_number_filenames () =
  if !number_filename != get_number_filename () then
    Util.Error.fail_error
      ("Error: a filenames containing the programs "
     ^ "should have been provided. " ^ usage_msg)

(* There is a tension between using the trick of
   Lts.Strategy.LTS with type conf = a,
   and the fact that this type Lts.Strategy.LTS.conf is not abstract *)

let build_graph (type a) (module Graph : Lts.Graph.GRAPH with type conf = a)
    (init_conf : a) =
  let show_conf conf_str =
    print_endline
      "Do you want to print the Proponent configuration? (1=yes/0=no)";
    let i = read_int () in
    match i with 1 -> print_endline @@ conf_str | _ -> () in
  let show_moves_list results_list =
    print_endline "The possible moves are:";
    List.iter print_endline
      (List.mapi (fun i m -> string_of_int (i + 1) ^ ": " ^ m) results_list)
  in
  let get_move n =
    print_endline
      ("Choose an integer between 1 and " ^ string_of_int n
     ^ " to decide what to do, or choose 0 to stop.");
    let i = read_int () in
    if i > 0 && i <= n then i else exit 1 in
  let graph =
    Graph.compute_graph ~show_conf ~show_moves_list ~get_move init_conf in
  let graph_string = Graph.string_of_graph graph in
  print_string graph_string

let build_ogs_lts (module IntLang : Lang.Interactive.LANG_WITH_INIT) =
  let (module OGS_LTS : Lts.Strategy.INT_LTS
        with type opconf = IntLang.opconf
         and type TypingLTS.Moves.Renaming.Namectx.t = IntLang.IEnv.Renaming.Namectx.t
         and type store = IntLang.store
         and type interactive_env = IntLang.IEnv.t) =
    match (!generate_tree, !enable_wb, !enable_visibility) with
    | (true, _, _) -> (module Pogs.Pogslts.Make (IntLang))
    | (false, true, true) ->
        let module TypingLTS = Ogs.Typing.Make (IntLang) in
        let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
        let module ProdLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
        let module VisLTS = Ogs.Vis_lts.Make (TypingLTS.Moves) in
        let module ProdLTS = Lts.Product_lts.Make (ProdLTS) (VisLTS) in
        (module Ogs.Ogslts.Make (IntLang) (ProdLTS))
    | (false, true, false) ->
        let module TypingLTS = Ogs.Typing.Make (IntLang) in
        let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
        let module ProdLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
        (module Ogs.Ogslts.Make (IntLang) (ProdLTS))
    | (false, false, true) ->
        let module TypingLTS = Ogs.Typing.Make (IntLang) in
        let module VisLTS = Ogs.Vis_lts.Make (TypingLTS.Moves) in
        let module ProdLTS = Lts.Product_lts.Make (TypingLTS) (VisLTS) in
        (module Ogs.Ogslts.Make (IntLang) (ProdLTS))
    | (false, false, false) ->
        let module TypingLTS = Ogs.Typing.Make (IntLang) in
        (module Ogs.Ogslts.Make (IntLang) (TypingLTS)) in
  check_number_filenames ();
  match !is_mode with
  | Compare -> begin
      let inBuffer1 = open_in !filename1 in
      let lexBuffer1 = Lexing.from_channel inBuffer1 in
      let (opconf1, namectxO1) = IntLang.get_typed_opconf "first" lexBuffer1 in
      Util.Debug.print_debug "Getting the second program";
      let inBuffer2 = open_in !filename2 in
      let lexBuffer2 = Lexing.from_channel inBuffer2 in
      let (opconf2, namectxO2) = IntLang.get_typed_opconf "second" lexBuffer2 in
      Util.Debug.print_debug
        ("Name contexts for Opponent: "
        ^ IntLang.IEnv.Renaming.Namectx.to_string namectxO1
        ^ " and "
        ^ IntLang.IEnv.Renaming.Namectx.to_string namectxO2);
      (* TODO: We should check that they are equal*)
      let module Synch_LTS = Lts.Synch_lts.Make (OGS_LTS) in
      let init_conf =
        Synch_LTS.Active (Synch_LTS.init_aconf (opconf1, opconf2) namectxO1)
      in
      if !print_dot then
        let module Graph = Lts.Graph.Make (Synch_LTS) in
        build_graph (module Graph) init_conf
      else
        let module Generate = Lts.Generate_trace.Make (Synch_LTS) in
        build_graph (module Generate) init_conf
    end
  | Explore ->
      if !is_program then begin
        Util.Debug.print_debug "Getting the program";
        let expr_buffer = open_in !filename1 in
        let expr_lexbuffer = Lexing.from_channel expr_buffer in
        let (opconf, namectxO) =
          IntLang.get_typed_opconf "first" expr_lexbuffer in
        (*Util.Debug.print_debug
          ("Name contexts for Opponent: " ^ IntLang.string_of_name_ctx namectxO);*)
        let init_conf = OGS_LTS.Active (OGS_LTS.init_aconf opconf namectxO) in
        if !print_dot then
          let module Graph = Lts.Graph.Make (OGS_LTS) in
          build_graph (module Graph) init_conf
        else
          let module Generate = Lts.Generate_trace.Make (OGS_LTS) in
          build_graph (module Generate) init_conf
      end
      else begin
        Util.Debug.print_debug "Getting the module declaration";
        let decl_buffer = open_in !filename1 in
        let decl_lexbuffer = Lexing.from_channel decl_buffer in
        let signature_buffer = open_in !filename2 in
        let signature_lexbuffer = Lexing.from_channel signature_buffer in
        let (interactive_env, store, name_ctxP, name_ctxO) =
          IntLang.get_typed_ienv decl_lexbuffer signature_lexbuffer in
        let init_conf =
          OGS_LTS.Passive
            (OGS_LTS.init_pconf store interactive_env name_ctxP name_ctxO) in
        if !print_dot then
          let module Graph = Lts.Graph.Make (OGS_LTS) in
          build_graph (module Graph) init_conf
        else
          let module Generate = Lts.Generate_trace.Make (OGS_LTS) in
          build_graph (module Generate) init_conf
      end
  | Compose -> failwith "Compose is not yet implemented"

let () =
  Arg.parse speclist get_filename usage_msg;
  fix_mode ();
  let module OpLang = Refml.RefML.WithAVal (Util.Monad.ListB) in
  if !enable_cps then
    let module CpsLang = Lang.Cps.MakeComp (OpLang) () in
    let module IntLang = Lang.Interactive.Make (CpsLang) in
    build_ogs_lts (module IntLang)
  else
    let module DirectLang = Lang.Direct.Make (OpLang) in
    build_ogs_lts (module DirectLang)