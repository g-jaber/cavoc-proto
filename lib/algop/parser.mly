%{
  open Syntax
  open Types

type handler_case =
  | Retcase of id * computation
  | Opcase of opsymbol * (id * id * computation)

let handler_record hcs_l = 
  let (ocs_l, retcs) = 
  List.fold_left 
  (fun (ocs_l, ret) -> function 
    Retcase (id, comp) -> (ocs_l, [(id,comp)]) | Opcase (op, hdl) -> ((op, hdl)::ocs_l, ret))
  ([], []) hcs_l in 
  {ocs=List.rev ocs_l; ret=List.hd retcs}

%}


%token EOF
%token <int> INT
%token <Syntax.id> NAME
%token <Syntax.id> RVAR
%token <Syntax.id> LNAME
%token <Syntax.id> UNAME
%token EQ
%token PLUS MINUS MULT DIV
%token LAND LOR NOT
%token NEQ GREAT GREATEQ LESS LESSEQ
%token TRUE FALSE
%token LPAR RPAR LBRACE RBRACE COMMA COLON SEMICOLON
%token LET IN
%token FUN ARROW
%token UNIT
%token MATCH HANDLE WITH RETURN PERFORM 

%token PIPE 

%token TYPE OF OPERATION SARROW

%token TUNIT
%token TINT
%token TBOOL
%left MULT
(*%right ARROW

%left IN
%left SEMICOLON

%nonassoc EQ NEQ GREAT GREATEQ LESS LESSEQ
%left LOR
%left LAND
%left PLUS MINUS
%left MULT DIV
*)

%start <Declaration.implem_decl list> prog

%start <Syntax.computation> fullterm

%%

fullterm: term; EOF  { $1 }

prog: list_decl; EOF  { $1 }


decl:
  | TYPE LNAME EQ ty { TypeDecl ($2,$4) }
  | LET x=LNAME l=list_ident EQ e=term
    { ValDecl (x, List.fold_left (fun expr var -> Return (Lambda (var,expr))) e l) }
  | OPERATION op=LNAME COLON t1=ty SARROW t2=ty { OpDecl (op, Arity(t1, t2)) }


list_decl:
  |  { [] }
  | list_decl decl {$2::$1}

subvalue: 
  | x=LNAME {Var x}
  | n=NAME            { Name (Names.fname_of_id (Names.trim_name_id n)) }
  | UNIT            { Unit }
  | i=INT             { Int i }
  | TRUE            { Bool true }
  | FALSE           { Bool false }
  | c=UNAME { Constructor (c, [])}
  | LPAR c=UNAME l=cons_params(subvalue) RPAR { Constructor (c, l) }
  | LPAR lab=LNAME v=subvalue RPAR  { Variant (lab, v) }
  | LPAR FUN l=list_ident ARROW t=term RPAR
     { match (List.fold_left (fun term var -> Return (Lambda (var,term))) t l )
       with Return v -> v | _ -> failwith "Parsing error"}

value:
  | x=LNAME {Var x}
  | n=NAME            { Name (Names.fname_of_id (Names.trim_name_id n)) }
  | UNIT            { Unit }
  | i=INT             { Int i }
  | TRUE            { Bool true }
  | FALSE           { Bool false }
  | c=UNAME { Constructor (c, [])}
  | c=UNAME l=cons_params(subvalue) { Constructor (c, l) }
  | subvalue PLUS value     { Binop (Plus, $1, $3) }
  | subvalue MINUS value    { Binop (Minus, $1, $3) }
  | subvalue MULT value     { Binop (Mult, $1, $3) }
  | subvalue DIV value      { Binop (Div, $1, $3) }
  | subvalue LAND value     { Binop (And, $1, $3) }
  | subvalue LOR value      { Binop (Or, $1, $3) }
  | subvalue EQ value      { Binop (Equal, $1, $3) }
  | subvalue NEQ value     { Binop (NEqual, $1, $3) }
  | subvalue GREAT value    { Binop (Great, $1, $3) }
  | subvalue GREATEQ value  { Binop (GreatEq, $1, $3) }
  | subvalue LESS value    { Binop (Less, $1, $3) }
  | subvalue LESSEQ value  { Binop (LessEq, $1, $3) }
  | LBRACE l=labelled(EQ, value) RBRACE { Record l }
  | FUN l=list_ident ARROW t=term 
     { match (List.fold_left (fun term var -> Return (Lambda (var,term))) t l )
       with Return v -> v | _ -> failwith "Parsing error"}
  | lab=LNAME v=subvalue   { Variant (lab, v) }

term: 
  | RETURN v=value 
    { Return v }
  | LET x=LNAME l=list_ident EQ t1=term IN t2=term
    { Let (x, List.fold_left (fun t var -> Return (Lambda (var,t))) t1 l, t2) }
  | MATCH v=value WITH LBRACE mcs=cases(match_case) RBRACE 
    { Match (v, mcs) }
  | f=subvalue v=subvalue
    { App (f, v) }
  | HANDLE t=term WITH LBRACE hcs=cases(handler_case) RBRACE 
    { Handle (t, handler_record hcs) }
  | PERFORM op=LNAME v=value 
    { Perform (op, v) }
  | LPAR term RPAR 
     { $2 }
 
subpattern: 
  | LNAME {PatVar $1}
  | n=NAME            { PatName (Names.fname_of_id (Names.trim_name_id n)) }
  | UNIT            { PatUnit }
  | i=INT             { PatInt i }
  | TRUE            { PatBool true }
  | FALSE           { PatBool false }
  | c=UNAME {PatCons (c, [])}
  | LPAR c=UNAME l=cons_params(subpattern) RPAR {PatCons (c, l)}
  | LPAR lab=LNAME p=subpattern RPAR { PatVariant (lab, p) }

pattern : 
  | LNAME {PatVar $1}
  | n=NAME            { PatName (Names.fname_of_id (Names.trim_name_id n)) }
  | UNIT            { PatUnit }
  | i=INT             { PatInt i }
  | TRUE            { PatBool true }
  | FALSE           { PatBool false }
  | c=UNAME {PatCons (c, [])}
  | c=UNAME l=cons_params(subpattern) {PatCons (c, l)}
(*  | LBRACE l=labelled(EQ, pattern) RBRACE { PatRecord l } *)
  | lab=LNAME p=subpattern { PatVariant (lab, p) }


cons_params(X): l=X+ { l }


labelled(SEP, X):
  | {[]}
  | lab=LNAME SEP x=X SEMICOLON l=labelled(SEP, X)
     { (lab,x)::l }

typed_ident:
  | UNIT { let var = fresh_evar () in (var,TUnit) }
  | x=LNAME { (x,TUndef) }
  | LPAR LNAME COLON ty RPAR { ($2,$4) }

list_ident:
  |  { [] }
  | list_ident typed_ident {$2::$1}

cases(case):
  | PIPE? cs = separated_list(PIPE, case) { cs }

match_case:
  | p=pattern ARROW t=term { (p, t) }

handler_case:
  | RETURN x=LNAME ARROW t=term { Retcase (x, t) }
  | op=LNAME x=LNAME k=LNAME ARROW t=term { Opcase (op, (x, k, t)) }


ty:
  | TUNIT        { TUnit }
  | TBOOL        { TBool }
  | TINT         { TInt }
  | ty MULT ty   { TProd ($1, $3) }
  | LPAR ty RPAR { $2 } 
  | LBRACE l=labelled(SEMICOLON, ty) RBRACE { TRecord l }
(*  
  | ty ARROW cty { TArrow ($1, $3) }

cty: 
  | typ=ty BANG LBRACE r=row RBRACE { Tcomp (typ, r) }

row:
  | l = seperated_list(COMMA, LNAME) PIPE TVAR 
    { {effects= l, poly=} }
*) 



%%
