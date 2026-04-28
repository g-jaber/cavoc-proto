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
      Tuple [ Arg.Clear enable_cps; Arg.Clear enable_wb ],
      "Use a representation of actions as calls and return rather than in cps \
       style. This is incompatible with both visibility restriction and open \
       composition." );
  ]

let usage_msg = "Usage: explore filename.ml filename.mli [options]"

let generate_kind_lts () =
  let open Lts_kind in
  let oplang = RefML in
  let control = if !enable_cps then CPS else DirectStyle in
  let restrictions = if !enable_wb then [ WellBracketing ] else [] in
  let restrictions =
    if !enable_visibility then Visibility :: restrictions else restrictions
  in
  { oplang; control; restrictions }

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

module Output = Util.Monad.Output (struct
    type t = string

    let show str = str
end)

(* There is a tension between using the trick of
   Lts.Strategy.LTS with type conf = a,
   and the fact that this type Lts.Strategy.LTS.conf is not abstract *)

let run_interaction (type a) (module IBuild : Lts.Interactive_build.IBUILD with type conf = a)
    (init_conf : a) =
  (* ask a question with a range of possible answers (rangestr).
     f is of type `(unit -> unit) -> string -> unit` and gets passed
     a function that can be used to ask the question again, and the
     line that has just been read *)
  let rec ask rangestr f =
    Printf.printf "(%s/exit) %!" rangestr;
    match String.trim (read_line ()) with
    | (exception End_of_file) | "exit" -> exit 0
    | line ->
        let askagain = fun () -> ask rangestr f in
        f askagain line in
  let show_move move = print_endline move in
  let show_conf conf_json =
    print_endline "Do you want to print the Proponent configuration?";
    let aux askagain = function
      | "yes" -> print_endline @@ Yojson.Safe.pretty_to_string conf_json
      | "no" -> ()
      | _ -> askagain () in
    ask "yes/no" aux in
  let show_moves_list results_list =
    let string_of_yojson_move (v : Yojson.Safe.t) =
      match v with
      | `Assoc fields -> (
          match List.assoc_opt "string" fields with
          | Some (`String str) -> str
          | _ ->
              failwith
                "The yojson encoding of the move does not have a string field. \
                 Please report.")
      | _ ->
          failwith
            "The yojson encoding of the move is not an Assoc. Please report."
    in
    print_endline "The possible moves are:";
    List.iter print_endline
      (List.mapi
         (fun i m -> string_of_int (i + 1) ^ ": " ^ string_of_yojson_move m)
         results_list) in
  let get_move n =
    let n = n + 1 in
    print_endline
      ("Choose an integer between 1 and " ^ string_of_int n
     ^ " to decide what to do, or type 'exit' to stop.");
    let aux askagain line =
      let i =
        try int_of_string line
        with Failure _ ->
          if line <> "" then print_endline "invalid integer";
          askagain () in
      if i > 0 && i <= n then i - 1
      else (
        print_endline "choice out of range";
        askagain ()) in
    IBuild.M.return  @@ ask (Printf.sprintf "1..%d" n) aux in
  let _result =
    IBuild.interactive_build ~show_move ~show_conf ~show_moves_list ~get_move
      init_conf in ()
(**  let trace = Graph.M.get_trace result in
  let graph_string = Graph.string_of_graph graph in
  print_string graph_string*) 

let open_lexbuf filename =
  let inBuffer = open_in filename in
  let exprBuffer = Lexing.from_channel inBuffer in
  Lexing.set_filename exprBuffer filename;
  exprBuffer

let build_strategy (module LTS : Lts.Strategy.LTS_WITH_INIT) =
  check_number_filenames ();
  match !is_mode with
  | Compare -> begin
      let exprBuffer1 = open_lexbuf !filename1 in
      let exprBuffer2 = open_lexbuf !filename2 in
      let module Synch_LTS = Lts.Synch_lts.Make (LTS) in
      let init_conf =
        Synch_LTS.Active (Synch_LTS.lexing_init_aconf exprBuffer1 exprBuffer2)
      in
      let module IBuild = Lts.Interactive_build.Make (Output) (Synch_LTS) in
    run_interaction (module IBuild) init_conf
    end
  | Explore ->
      if !is_program then begin
        Util.Debug.print_debug "Getting the program";
        let expr_lexbuffer = open_lexbuf !filename1 in
        let init_conf = LTS.Active (LTS.lexing_init_aconf expr_lexbuffer) in
        let module IBuild = Lts.Interactive_build.Make (Output) (LTS) in
        run_interaction (module IBuild) init_conf
      end
      else begin
        Util.Debug.print_debug "Getting the module declaration";
        let decl_lexbuffer = open_lexbuf !filename1 in
        let signature_lexbuffer = open_lexbuf !filename2 in
        let init_conf =
          LTS.Passive (LTS.lexing_init_pconf decl_lexbuffer signature_lexbuffer)
        in
        let module IBuild = Lts.Interactive_build.Make (Output) (LTS) in
        run_interaction (module IBuild) init_conf
      end
  | Compose -> failwith "Compose is not yet implemented"

let () =
  Arg.parse speclist get_filename usage_msg;
  fix_mode ();
  let kind_lts = generate_kind_lts () in
  let lts = Lts_kind.build_lts kind_lts in
  build_strategy lts
