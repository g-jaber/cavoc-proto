open Arg

let get_prog nbprog filename =
  let inBuffer = open_in filename in
  let lineBuffer = Lexing.from_channel inBuffer in
  try let prog = Cavoc.Parser.prog Cavoc.Lexer.token lineBuffer in
    Cavoc.Debug.print_debug (nbprog ^ " program: " ^ Cavoc.Declaration.string_of_program prog);
    prog
    (*
    let ty = Cavoc.Type_checker.typing_full poly_tvar int_tvar  in
    Cavoc.Debug.print_debug ("His type is: " ^ Cavoc.Syntax.string_of_typeML ty);
    (expr,ty) *)
  with
  | Cavoc.Lexer.SyntaxError msg -> failwith ("Parsing Error in the " ^ nbprog
                                       ^ " program:" ^ msg)
  | Cavoc.Parser.Error ->
    failwith ("Syntax Error in the " ^ nbprog ^ " program:"
              ^ " at position "
              ^ (string_of_int (Lexing.lexeme_start lineBuffer)))
  | Cavoc.Type_checker.TypingError msg -> failwith ("Type error :" ^ msg)

let () =
  let number_filename = ref 0 in
  let filename = ref "" in
  let ext_reason = ref true in
  let speclist =
    [("-debug",Set Cavoc.Debug.debug_mode,"Debug mode");
     ("-no-ext-reason", Clear ext_reason, "Forbid the initial extensional reasoning on values");
    ] in
  let usage_msg = "Usage: cavoc filename [options]" in
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
  let _ = get_prog "first" !filename
in ();;