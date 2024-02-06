{
  open Lexing
  open Parser

  exception SyntaxError of string

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <-
        { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z'] (alpha | '_' | '\'' | digit)*
let name = '_' (alpha | '_' | '\'' | digit)*
let rvar = '\'' (alpha | '_' | digit)*
let integer = digit+
let upper = ['A'-'Z'] (alpha | '_' | '\'' | digit)*
let lower = ['a'-'z'] (alpha | '_' | '\'' | digit)*

rule token = parse
  | '\n'  { newline lexbuf; token lexbuf }
  | space  { token lexbuf }
  | "(*"  { comment 0 lexbuf }

  | '=' { EQ }

  | '+'  { PLUS }
  | '-'  { MINUS }
  | '*'  { MULT }
  | '/'  { DIV }

  | "true" { TRUE }
  | "false" { FALSE }
  | "&&" { LAND }
  | "||" { LOR }
  | "not" { NOT }
  | ">" { GREAT }
  | ">=" { GREATEQ }
  | "<" { LESS }
  | "<=" { LESSEQ }
  | "<>" { NEQ }
  | "!=" { NEQ }

  | "let"  { LET }
  | "in"  { IN }
  | "fun"  { FUN }
  | "match" { MATCH }
  | "with"  { WITH }
  | "handle" { HANDLE }
  | "|" { PIPE }
  | "return" { RETURN }
  | "perform" { PERFORM }

  | "()" { UNIT }


  | "unit" { TUNIT }
  | "int" { TINT }
  | "bool" { TBOOL }

  | "->"  { ARROW }
  | "~>"  { SARROW }

  | '('  { LPAR }
  | ')'  { RPAR }
  | '{'  { LBRACE }
  | '}'  { RBRACE }
  | ','  { COMMA }
  | ':'  { COLON }
  | ';'  { SEMICOLON }
  
  | "type" { TYPE }  
  | "operation" { OPERATION }
  | "of" { OF }

  | eof  { EOF }


  | integer as n  { INT (int_of_string n) }
  | rvar as t  { RVAR t }
  | upper as un { UNAME un }
  | lower as ln { LNAME ln }


  | _  { raise Error }

and comment depth = parse
  | '\n'  { newline lexbuf; comment depth lexbuf }
  | "(*"  { comment (depth + 1) lexbuf }
  | "*)"
    {
      match depth with
      | 0 -> token lexbuf
      | _ -> comment (depth - 1) lexbuf
    }
  | eof     { raise Error }
  | _       { comment depth lexbuf }
