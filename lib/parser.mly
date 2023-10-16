%{
  open Syntax
  open Types
  open Declaration
%}


%token EOF
%token <int> INT
%token <Syntax.id> VAR
%token EQ
%token PLUS MINUS MULT DIV
%token LAND LOR NOT
%token NEQ GREAT GREATEQ LESS LESSEQ
%token TRUE FALSE
%token LPAR RPAR COMMA COLON SEMICOLON
%token LET REC IN
%token FUN FIX ARROW
%token IF THEN ELSE
%token UNIT
%token REF ASSIGN DEREF
%token WHILE DO DONE
%token MATCH WITH
%token ASSERT

%token TYPE VAL

%token TUNIT
%token TINT
%token TBOOL

%left ELSE IN ARROW
%left SEMICOLON
%left ASSIGN
%nonassoc NOT
%nonassoc EQ NEQ GREAT GREATEQ LESS LESSEQ
%left LOR
%left LAND
%left PLUS MINUS
%left MULT DIV
%nonassoc REF
%nonassoc ASSERT


%start prog
%type <Declaration.implem_decl list> prog

%start signature
%type <Declaration.signature_decl list> signature

%start fullexpr
%type <Syntax.exprML> fullexpr

%%

fullexpr: expr; EOF  { $1 }

prog: list_implem_decl; EOF  { $1 }

signature: list_signature_decl; EOF { $1 }

signature_decl:
  | TYPE VAR { PrivateTypeDecl ($2) }  
  | TYPE VAR EQ ty { PublicTypeDecl ($2,$4) }
  | VAL VAR COLON ty { PublicValDecl ($2,$4) }

implem_decl:
  | TYPE VAR EQ ty { TypeDecl ($2,$4) }
  | LET VAR list_ident EQ expr
    { ValDecl ($2, List.fold_left (fun expr var -> Fun (var,expr)) $5 $3) }
  | LET REC VAR typed_ident list_ident EQ expr
    { ValDecl ($3, Fix (($3,TUndef),$4, List.fold_left (fun expr var -> Fun (var,expr)) $7 $5)) }

list_signature_decl:
  |  { [] }
  | list_signature_decl signature_decl {$2::$1}

list_implem_decl:
  |  { [] }
  | list_implem_decl implem_decl {$2::$1}


expr:
  | app_expr { $1 }
  | expr SEMICOLON expr         { Seq ($1, $3) }
  | IF expr THEN expr ELSE expr        { If ($2, $4, $6) }
  | FUN typed_ident ARROW expr { Fun ($2, $4) }
  | FIX typed_ident typed_ident ARROW expr
    { Fix ($2,$3, $5) }
  | LET VAR list_ident EQ expr IN expr
    { Let ($2, List.fold_left (fun expr var -> Fun (var,expr)) $5 $3, $7) }
  | LET REC VAR typed_ident list_ident EQ expr IN expr
    { Let ($3, Fix (($3,TUndef),$4, List.fold_left (fun expr var -> Fun (var,expr)) $7 $5), $9) }
  | LET LPAR VAR COMMA VAR RPAR EQ expr IN expr
    { LetPair ($3,$5,$8,$10)}
  | WHILE expr DO expr DONE { While ($2,$4) }
  | REF expr         { Newref (TUndef,$2) }
  | expr ASSIGN expr { Assign ($1,$3) }
  | ASSERT expr      { Assert $2 }
(*  | MINUS expr          { UMinus (-$2) }  *)
  | expr PLUS expr     { BinaryOp (Plus, $1, $3) }
  | expr MINUS expr    { BinaryOp (Minus, $1, $3) }
  | expr MULT expr     { BinaryOp (Mult, $1, $3) }
  | expr DIV expr      { BinaryOp (Div, $1, $3) }
  | NOT expr          { UnaryOp (Not, $2) }
  | expr LAND expr     { BinaryOp (And, $1, $3) }
  | expr LOR expr      { BinaryOp (Or, $1, $3) }
  | expr EQ expr      { BinaryOp (Equal, $1, $3) }
  | expr NEQ expr     { BinaryOp (NEqual, $1, $3) }
  | expr GREAT expr    { BinaryOp (Great, $1, $3) }
  | expr GREATEQ expr  { BinaryOp (GreatEq, $1, $3) }
  | expr LESS expr    { BinaryOp (Less, $1, $3) }
  | expr LESSEQ expr  { BinaryOp (LessEq, $1, $3) }

app_expr:
  | simple_expr { $1 }
  | app_expr simple_expr         { App ($1, $2) }

simple_expr:
  | VAR             { Var $1 }
  | UNIT            { Unit }
  | INT             { Int $1 }
  | TRUE            { Bool true }
  | FALSE           { Bool false }
  | LPAR expr COMMA expr RPAR   { Pair ($2, $4) }
  | DEREF VAR       { Deref (Var $2) }
  | LPAR expr RPAR   { $2 }

typed_ident:
  | UNIT { let var = fresh_evar () in (var,TUnit) }
  | VAR { ($1,TUndef) }
  | LPAR VAR COLON ty RPAR { ($2,$4) }

list_ident :
  |  { [] }
  | list_ident typed_ident {$2::$1}

ty:
  | VAR          { TVar $1 }
  | TUNIT        { TUnit }
  | TBOOL        { TBool }
  | TINT         { TInt }
  | REF ty      { TRef $2 }
  | ty ARROW ty { TArrow ($1, $3) }
  | ty MULT ty   { TProd ($1, $3) }
  | LPAR ty RPAR { $2 }

%%
