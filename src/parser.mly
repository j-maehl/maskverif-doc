%{ 
  open Util
  open Parsetree
  let parse_error loc msg = raise (ParseError (loc, msg))

%}
%token PROC INPUTS OUTPUTS SHARES RANDOMS

%token <string> IDENT
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LCURLY
%token RCURLY
%token COMMA SEMICOLON COLON
%token DOTEQ EQ 
%token ADD MUL NOT
%token EOF
%left ADD
%left MUL
%nonassoc NOT

%start prog
%type <Parsetree.prog> prog

%%

%inline loc(X):
| x=X { { pl_data = x; pl_location = Location.make $startpos $endpos; } }
;

ident:
  | x=loc(IDENT) { x }

expr_r: 
  | x=ident                  { Evar x }
  | LPAREN e=expr RPAREN     { data e }
  | e1=expr ADD e2=expr      { Eadd(e1, e2) }
  | e1=expr MUL e2=expr      { Emul(e1, e2) }
  | NOT e=expr               { Enot e }

expr:
  | e=loc(expr_r)            { e }

%inline assgn: 
  | x=ident DOTEQ e=expr 
    { {i_var = x; i_kind = IK_subst; i_expr = e } }
  | x=ident EQ e=expr  
    { {i_var = x; i_kind = IK_sub; i_expr = e } }
  | x=ident EQ LBRACKET e=expr RBRACKET 
    { {i_var = x; i_kind = IK_glitch; i_expr = e } }
  | x=ident EQ LCURLY e=expr RCURLY     
    { {i_var = x; i_kind = IK_hide; i_expr = e } }
  
%inline vcall:
  | x=ident { Vid x }
  | LBRACKET xs=loc(separated_list(COMMA,ident)) RBRACKET { Vtuple xs }

%inline vcalls:
  | x = vcall { [x] }
  | LPAREN xs=separated_list(COMMA,vcall) RPAREN { xs }

%inline call:
  | xs=vcalls EQ f=ident ys=vcalls { {i_lhs = xs; i_macro = f; i_args = ys } }

instr: 
  | i=assgn SEMICOLON { Iassgn i }
  | i=call  SEMICOLON { Imacro i }

cmd: 
  | c=list(instr) { c }

shares:
  | xs=separated_list (ADD, ident) {xs}

decl:
  | x=ident EQ xs=shares  { (x, xs) }

decls:
  | ds=separated_list(COMMA,decl) { ds }

inputs:
  | INPUTS COLON d=decls { d }

outputs:
  | OUTPUTS COLON d=decls { d }

shares_decl:
  |                      { [] }
  | SHARES COLON d=decls { d }
 
randoms:
  |                                             { [] }
  | RANDOMS COLON r=separated_list(COMMA,ident) { r }

fname:
  | PROC f=ident COLON { f }

func:
  | f=fname i=inputs o=outputs s=shares_decl r=randoms SEMICOLON c=cmd 
    { {f_name=f; f_in = i; f_out = o; f_shares = s; f_rand = r; f_cmd = c } }

prog:
  | fs=list(func) EOF { fs }
  | error { parse_error (Location.make $startpos $endpos) None } 


  


  
