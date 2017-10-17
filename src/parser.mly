%{ 
  open Parsetree
  let parse_error loc msg = raise (ParseError (loc, msg))

%}
%token <string> VIDENT
%token <int> INT


%token SHARED VAR
%token RETURN 
%token SNI NI

%token DOLLAR EQ
%token ADD MUL
%token SEMICOLON COMMA
%token LPAREN 
%token RPAREN
%token LBRACKET
%token RBRACKET

%type <Parsetree.pkind * Parsetree.pprog> global
%start global
%%

%inline loc(X):
| x=X { { pl_data = x; pl_location = Location.make $startpos $endpos; } }
;

int:
| i=INT { i }

vident:
| x=loc(VIDENT) { x }

sharedident:
| x=vident       { `Var x }
| x=vident i=int { `Share(x,i) }

vars_decl:
| SHARED xs=vident+ SEMICOLON { List.map (fun x -> `Shared x) xs }
| VAR    xs=vident+ SEMICOLON { List.map (fun x -> `Single x) xs }

op:
| ADD { `Add }
| MUL { `Mul }

expr:
| DOLLAR                             { `Rnd          }
| x=sharedident                      { `Allias x     }
| x1=sharedident o=op x2=sharedident { `Op(x1,o,x2) }

command:
| x=sharedident EQ e=expr SEMICOLON { { lhs = x; rhs = e } }

return_expr:
| x=vident                                            { `Shared x }
| LPAREN xs=separated_list(COMMA, sharedident) RPAREN { `Tuple xs }

return:
| RETURN e=loc(return_expr) SEMICOLON { e }

prog_body:
| LBRACKET vs=vars_decl* c=command* r=return RBRACKET { List.flatten vs, c, r }
  
sni:
| NI  { `NI }
| SNI ki=int? { `SNI ki}


global:
| k=sni prog=vident LPAREN params=vident+ nbshares = int RPAREN b=prog_body 
  { let (d,c,r) = b in
    k, { p_name     = prog;
         p_params   = params;
         p_nbshares = nbshares;
         p_decl     = d;
         p_body     = c;
         p_return   = r } }

| error
    { parse_error (Location.make $startpos $endpos) None }
;

