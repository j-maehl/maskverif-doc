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

%token WIRE INPUT OUTPUT WIDTH 
%token CELL
%token CONNECT

%token LBRACKET RBRACKET
%token LCURLY RCURLY
%token COLON
%token EOF

%token SHARPSHARP
%token RANDOM PUBLIC

%start prog
%type <Ilang_ast.prog> prog

%%

%inline loc(X):
| x=X { { pl_data = x; pl_location = Location.make $startpos $endpos; } }

ident:
  | x=loc(IDENT) { x }



/* parse extra stuff */

id_index:
  | x=ident                         { x, None }
  | x=ident LBRACKET i=INT RBRACKET { x, Some i }

share_decl:
  | x=ident                                     { Sident x }
  | x=ident LBRACKET i=INT COLON j=INT RBRACKET { Sindex(x,i,j) }
  | x=ident LBRACKET ns=INT+ RBRACKET           { Svindex(x,ns) }
  | LBRACKET xs=id_index+ RBRACKET              { Svect(xs) }

name_decl:
  | COLON x=ident                             { x }

maskverif_decl:
  | SHARPSHARP INPUT  x=name_decl? s=share_decl  { Input (x,s) }
  | SHARPSHARP OUTPUT s=share_decl           { Output s }
  | SHARPSHARP RANDOM xs=id_index*           { Random xs }
  | SHARPSHARP PUBLIC xs=id_index*           { Public xs }

attribute_arg:
  | i=INT    { AA_int i }
  | s=STRING { AA_string s}
  | c=CONST  { AA_string c }

attribute:
  | ATTRIBUTE k=ident s=attribute_arg { k, s }

with_attribute(X):
  | la=list(attribute) x=X { { attribute = la; wa_data = x} }

wire_opt:
  | INPUT  i=INT { WO_input  i}
  | OUTPUT i=INT { WO_output i}
  | WIDTH  i=INT { WO_width  i}

index:
  | LBRACKET i=INT COLON j=INT RBRACKET { i,j }
  | LBRACKET i=INT RBRACKET             { i,i }

connect_expr1:
  | i=id_index   { Eid i }
  | c=CONST      { Econst c }

connect_exprn:
  | e=connect_expr1 { [e] }
  | x=ident LBRACKET i=INT COLON j=INT RBRACKET 
    { List.map (fun i -> Eid (x,Some i)) (mk_range_i i j) }

connect_expr:
  | e=connect_expr1                        { Rexpr e }
  | LCURLY es=list(connect_exprn) RCURLY   { Rvect (List.flatten es) }

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
  | WIRE o=list(wire_opt) x=ident  { Wire(o,x) }
  | c=cell                         { Cell c }
  | c=connect                      { Connect c }
  | d=maskverif_decl               { Decl d }

module1:
  | MODULE x=ident ld=list(with_attribute(decl)) END { 
    { mod_name = x; mod_decl = ld} }

prog:
  | AUTOIDX n=INT 
    m=with_attribute(module1) EOF { 
          { pautoidx = n; module_decl = m} }
  | error { parse_error (Location.make $startpos $endpos) None }
 
