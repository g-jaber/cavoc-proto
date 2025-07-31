%{
  open Syntax
  open Types
  open Declaration
%}


%token EOF
%token <int> INT
%token <Syntax.id> VAR
%token <Syntax.id> TVAR
%token <Syntax.id> NAME
%token <Syntax.constructor> CONSTRUCTOR
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
%token WITH
%token ASSERT
%token RAISE
%token TRY
%token PIPE

%token TYPE VAL EXCEPTION OF

%token TUNIT
%token TINT
%token TBOOL
%token TEXN
%right ARROW

%left ELSE IN
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
%nonassoc RAISE

%start prog
%type <Declaration.implem_decl list> prog

%start signature
%type <Declaration.signature_decl list> signature

%start fullexpr
%type <Syntax.term> fullexpr


%%

fullexpr: 
| e=expr_with_try; EOF  { e }

prog: 
| l=list_implem_decl; EOF  { l }

signature: 
| l=list_signature_decl; EOF { l }

signature_decl:
  | TYPE v=VAR { PrivateTypeDecl (v) }  
  | TYPE v=VAR EQ t=ty { PublicTypeDecl (v,t) }
  | VAL v=VAR COLON t=ty { PublicValDecl (v,t) }
  | EXCEPTION c=CONSTRUCTOR  { PublicExnDecl (c, None) }
  | EXCEPTION c=CONSTRUCTOR OF t=ty { PublicExnDecl (c, Some t) }

implem_decl:
  | TYPE v=VAR EQ t=ty { TypeDecl (v,t) }
  | LET v=VAR l=list_ident EQ e=expr_with_try
    { ValDecl (v, List.fold_left (fun expr var -> Fun (var,expr)) e l) }
  | LET REC v=VAR t=typed_ident l=list_ident EQ e=expr
    { ValDecl (v, Fix ((v,TUndef),t, List.fold_left (fun expr_with_try var -> Fun (var,expr_with_try)) e l)) }
  | EXCEPTION c=CONSTRUCTOR { ExnDecl (c, None) }
  | EXCEPTION c=CONSTRUCTOR OF t=ty { ExnDecl (c, Some t) }

list_signature_decl:
  |  { [] }
  | l=list_signature_decl d=signature_decl {d::l}

list_implem_decl:
  |  { [] }
  | l=list_implem_decl d=implem_decl {d::l}

 pattern : 
   | c=CONSTRUCTOR v=VAR {PatCons (c, v)}
   | v=VAR {PatVar v}

 handler : PIPE p=pattern ARROW e=expr {p,e}
 handler_list : 
   | { [] }
   | hl=handler_list h=handler {(Handler h)::hl}

expr_with_try:
  | e=expr { e }
  | TRY e=expr WITH hl=handler_list { TryWith (e,hl) }


expr:
  | e=app_expr { e }
  | e1=expr SEMICOLON e2=expr         { Seq (e1, e2) }
  | IF e1=expr THEN e2=expr ELSE e3=expr        { If (e1, e2, e3) }
  | FUN tid=typed_ident ARROW e=expr { Fun (tid, e) }
  | FIX tid1=typed_ident tid2=typed_ident ARROW e=expr
    { Fix (tid1, tid2, e) }
  | LET v=VAR lid=list_ident EQ e1=expr IN e2=expr
    { Let (v, List.fold_left (fun expr var -> Fun (var,expr)) e1 lid, e2) }
  | LET REC v=VAR tid=typed_ident lid=list_ident EQ e1=expr IN e2=expr
    { Let (v, Fix ((v,TUndef),tid, List.fold_left (fun expr var -> Fun (var,expr)) e1 lid), e2) }
  | LET LPAR v1=VAR COMMA v2=VAR RPAR EQ e1=expr IN e2=expr
    { LetPair (v1,v2,e1,e2)}
  | WHILE e1=expr DO e2=expr DONE { While (e1,e2) }
  | REF e=expr         { Newref (TUndef,e) }
  | e1=expr ASSIGN e2=expr { Assign (e1,e2) }
  | ASSERT e=expr      { Assert e }
  | RAISE e=expr { Raise e }
(*  | MINUS e=expr          { UMinus e }  *)
  | e1=expr PLUS e2=expr     { BinaryOp (Plus, e1, e2) }
  | e1=expr MINUS e2=expr    { BinaryOp (Minus, e1, e2) }
  | e1=expr MULT e2=expr     { BinaryOp (Mult, e1, e2) }
  | e1=expr DIV e2=expr      { BinaryOp (Div, e1, e2) }
  | NOT e=expr          { UnaryOp (Not, e) }
  | e1=expr LAND e2=expr     { BinaryOp (And, e1, e2) }
  | e1=expr LOR e2=expr      { BinaryOp (Or, e1, e2) }
  | e1=expr EQ e2=expr      { BinaryOp (Equal, e1, e2) }
  | e1=expr NEQ e2=expr     { BinaryOp (NEqual, e1, e2) }
  | e1=expr GREAT e2=expr    { BinaryOp (Great, e1, e2) }
  | e1=expr GREATEQ e2=expr  { BinaryOp (GreatEq, e1, e2) }
  | e1=expr LESS e2=expr    { BinaryOp (Less, e1, e2) }
  | e1=expr LESSEQ e2=expr  { BinaryOp (LessEq, e1, e2) }



app_expr:
  | e=simple_expr { e }
  | e1=app_expr e2=simple_expr         { App (e1, e2) }

simple_expr:
  | v=VAR             { Var v }
  | c=CONSTRUCTOR p=simple_expr    { Constructor (c, p)}
  | n=NAME            { Name (Names.fname_of_id (Names.trim_name_id n)) }
  | UNIT            { Unit }
  | n=INT             { Int n }
  | TRUE            { Bool true }
  | FALSE           { Bool false }
  | LPAR e1=expr COMMA e2=expr RPAR   { Pair (e1, e2) }
  | DEREF v=VAR       { Deref (Var v) }
  | LPAR e=expr_with_try RPAR   { e }

typed_ident:
  | UNIT { let var = fresh_evar () in (var,TUnit) }
  | v=VAR { (v,TUndef) }
  | LPAR v=VAR COLON t=ty RPAR { (v,t) }

list_ident :
  |  { [] }
  | l=list_ident tid=typed_ident {tid::l}

ty:
  | v=TVAR          { TVar v }
  | v=VAR          { TId v }
  | TUNIT        { TUnit }
  | TBOOL        { TBool }
  | TINT         { TInt }
  | REF t=ty      { TRef t }
  | t1=ty ARROW t2=ty { TArrow (t1, t2) }
  | t1=ty MULT t2=ty   { TProd (t1, t2) }
  | LPAR t=ty RPAR { t }
  | TEXN { TExn }

%%
