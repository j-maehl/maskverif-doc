%{ 
  open Util
  open Parsetree
  let parse_error loc msg = raise (ParseError (loc, msg))

%}
%token PROC END PUBLIC INPUTS OUTPUTS SHARES RANDOMS
%token <string>SNI NI PROBING 
%token <string>READ_FILE READ_ILANG

%token <string> IDENT
%token <int> INT
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LCURLY
%token RCURLY
%token COMMA SEMICOLON COLON
%token MARK DOTEQ EQ 
%token LSR LSL
%token ADD MUL NOT
%token EOF
%left ADD
%left MUL
%nonassoc NOT

%start command 
%start file
%type <Parsetree.command> command
%type <Parsetree.command list> file

%%

%inline loc(X):
| x=X { { pl_data = x; pl_location = Location.make $startpos $endpos; } }
;

ident:
  | x=loc(IDENT) { x }

range1:
  | i=INT COLON j=INT { i, j }
  | i=INT             { i, i }

range:
  | LBRACKET r=range1 RBRACKET { r } 

%inline rangen: 
  | LBRACKET r=separated_list(COMMA,range1) RBRACKET { r }

shift:
  | LSR i=INT { Sr i }
  | LSL i=INT { Sl i }

%inline id_range:
  | x=ident r=rangen? { x,r }

id_range_decl:
  | x=ident r=range? { x, r }

%inline vcall1:
  | x=id_range { Vid x }
  | LBRACKET xs=loc(separated_list(COMMA,ident)) RBRACKET { Vtuple xs }

%inline vcall: 
  | x=vcall1 s=shift? { x, s }

expr_r: 
  | x=vcall               { Evar x }
  | LPAREN e=expr RPAREN     { data e }
  | e1=expr ADD e2=expr      { Eadd(e1, e2) }
  | e1=expr MUL e2=expr      { Emul(e1, e2) }
  | NOT e=expr               { Enot e }

expr:
  | e=loc(expr_r)            { e }

%inline assgn: 
  | x=vcall DOTEQ e=expr 
    { {i_var = x; i_kind = IK_subst; i_expr = e } }
  | x=vcall EQ e=expr  
    { {i_var = x; i_kind = IK_sub; i_expr = e } }
  | x=vcall EQ MARK LBRACKET e=expr RBRACKET
    { {i_var = x; i_kind = IK_glitch; i_expr = e } }
  | x=vcall EQ LCURLY e=expr RCURLY     
    { {i_var = x; i_kind = IK_hide; i_expr = e } }
  
%inline vcalls:
  | LPAREN xs=separated_list(COMMA,vcall) RPAREN { xs }

%inline lhs:
  | x=vcall { [x] }
  | LPAREN xs=separated_list(COMMA,vcall) RPAREN { xs }

call:
  | xs=lhs EQ f=ident ys=vcalls { {i_lhs = xs; i_macro = f; i_args = ys } }

instr: 
  | i=assgn SEMICOLON { Iassgn i }
  | i=call  SEMICOLON { Imacro i }

cmd: 
  | c=list(instr) { c }

shares:
  | xs=separated_list (ADD, ident) {xs}

decl:
  | x=ident EQ xs=shares  { (x, Ids xs) }
  | x=ident r=range { x, Range r }

decls:
  | ds=separated_list(COMMA,decl) { ds }

inputs:
  | INPUTS COLON d=decls { d }

outputs:
  | OUTPUTS COLON d=decls { d }

shares_decl:
  |                      { [] }
  | SHARES COLON d=decls { d }

pub_inputs:
  |                                                     { [] }
  | PUBLIC INPUTS d=separated_list(COMMA,id_range_decl) { d }
 
randoms:
  |                                                     { [] }
  | RANDOMS COLON r=separated_list(COMMA,id_range_decl) { r }

func:
  | PROC f_name=ident COLON 
      f_pin    = pub_inputs 
      f_in     = inputs 
      f_out    = outputs 
      f_shares = shares_decl 
      f_rand   = randoms SEMICOLON
      f_cmd    = cmd 
    END
    { { f_name; f_pin; f_in; f_out; f_shares; f_rand; f_cmd } }

sni_bound:
  | to_=INT          { 0,to_ }
  | from=INT COLON to_=INT { from, to_ }
command1:
  | f=func            { Func f }
  | f=loc(NI)         { NI f }
  | b=sni_bound? f=loc(SNI) { SNI (f,b) }
  | f=loc(PROBING)    { Probing f}
  | f=loc(READ_FILE)  { Read_file f  }
  | f=loc(READ_ILANG) { Read_ilang f }
  | error             { parse_error (Location.make $startpos $endpos) None }

command:
  | c=command1     { c }
  | EOF            { Exit }

file:
  | c=list(command1) EOF { c }


  
