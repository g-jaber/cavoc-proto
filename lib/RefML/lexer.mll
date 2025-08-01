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
let tvar = '\'' (alpha | '_' | digit)*
let integer = digit+
let constructor = ['A'-'Z'] (alpha | '_' | '\'' | digit)*

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
  | "rec" { REC }
  | "in"  { IN }
  | "fun"  { FUN }
  | "fix"  { FIX }
  | "with"  { WITH }
  | "assert" { ASSERT }
  | "while" { WHILE }
  | "do" { DO }
  | "done" { DONE }

  | "()" { UNIT }
  | "ref" { REF }
  | ":=" { ASSIGN }
  | "!" { DEREF }

  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }

  | "raise" { RAISE }
  | "try" { TRY }
  | "|" { PIPE }

  | "unit" { TUNIT }
  | "int" { TINT }
  | "bool" { TBOOL }
  | "exn" { TEXN }

  | "->"  { ARROW }

  | '('  { LPAR }
  | ')'  { RPAR }
  | ','  { COMMA }
  | ':'  { COLON }
  | ';'  { SEMICOLON }
  
  | "type" { TYPE }  
  | "val" { VAL } 
  | "exception" { EXCEPTION }
  | "of" { OF }

  | eof  { EOF }


  | integer as n  { INT (int_of_string n) }
  | ident as id  { VAR id }
  | tvar as t  { TVAR t }
  | name as nn { NAME nn }
  | constructor as c { CONSTRUCTOR c }

  | _  { failwith (Printf.sprintf "Unexpected char at %d" lexbuf.lex_curr_p.pos_cnum) }

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
