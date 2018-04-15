%{
    open Util
    open Ilang_ast
    let parse_error loc msg = raise (ParseError (loc, msg))

%}
%token AUTOIDX
%token ATTRIBUTE
%token MODULE END
%token <string>STRING
%token <string>IDENT
%token <int>INT
%token <string>CONST

%token WIRE RANDOM PUBLIC INPUT OUTPUT WIDTH SHARES
%token CELL
%token CONNECT

%token LBRACKET RBRACKET
%token LCURLY RCURLY
%token COLON
%token EOF
%start prog
%type <Ilang_ast.prog> prog

%%

%inline loc(X):
| x=X { { pl_data = x; pl_location = Location.make $startpos $endpos; } }

ident:
  | x=loc(IDENT) { x }

attribute_arg:
  | i=INT    { AA_int i }
  | s=STRING { AA_string s}
  | c=CONST  { AA_string c }

attribute:
  | ATTRIBUTE k=ident s=attribute_arg { k, s }

with_attribute(X):
  | la=list(attribute) x=X { { attribute = la; wa_data = x} }

shares_by:
  | SHARES nb=INT iby=INT { nb,iby }

wire_opt:
  | RANDOM INPUT i=INT { WO_random i }
  | PUBLIC INPUT i=INT { WO_public i}
  | INPUT  i=INT s=shares_by? { WO_input(i,s)}
  | OUTPUT i=INT s=shares_by? { WO_output(i,s)}
  | WIDTH  i=INT { WO_width i}

index:
  | LBRACKET i=INT COLON j=INT RBRACKET { i,j }
  | LBRACKET i=INT RBRACKET             { i,i }

index1:
  | LBRACKET i=INT RBRACKET             { i }

ident_index:
  | x=ident i=index1? { x,i} 

connect_expr1:
  | i=ident_index   { Eid i }
  | c=CONST         { Econst c }

connect_expr:
  | e=connect_expr1                        { Rexpr e }
  | LCURLY es=list(connect_expr1) RCURLY   { Rvect es }

c_connect:
  | CONNECT lhs=ident rhs=connect_expr1 { 
     { c_connect_lhs = lhs; c_connect_rhs = rhs } }

cell:
  | CELL i1=ident i2=ident cs=list(c_connect) END {
     { cell_name1 = i1; cell_name2 = i2; cell_connect = cs } }

connect:
  | CONNECT x=ident i=index? rhs=connect_expr { 
     { connect_lhs = (x,i); connect_rhs = rhs } }

decl:
  | WIRE o=list(wire_opt) x=ident { Wire(o,x) }
  | c=cell                        { Cell c }
  | c=connect                     { Connect c }

module1:
  | MODULE x=ident ld=list(with_attribute(decl)) END { 
    { mod_name = x; mod_decl = ld} }

prog:
  | AUTOIDX n=INT m=with_attribute(module1) EOF { 
    { pautoidx = n; module_decl = m} }
  | error { parse_error (Location.make $startpos $endpos) None }
 
