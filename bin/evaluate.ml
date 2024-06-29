open Opalg

let parse_program s =
  (*let ppf = Format.err_formatter in *)
  let lexbuf = Lexing.from_string s in
  Parser.fullterm Lexer.token lexbuf
(*try Parser.fullterm Lexer.token lexbuf with
  | Error.Parsing err -> Error.report_parsing ppf err; exit 1
  | Error.Lexing err -> Error.report_lexing ppf err; exit 1 *)

let rec parprint _ =
  Printf.printf "$> ";
  flush stdout;
  let input = read_line () in
  let parsed_term = parse_program input in
  let output = Syntax.string_of_computation parsed_term in
  print_endline output;

  Printf.printf "  ~~~>  ";
  flush stdout;
  let init_conf = Interpreter.inj_config parsed_term in
  let nf = Interpreter.abstract_machine init_conf in
  let output = Syntax.string_of_computation nf.term in
  print_endline output;
  print_endline "";

  parprint ()

let _ = parprint ()
