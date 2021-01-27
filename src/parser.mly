%{
  open Maskverif
  open Util
  open Parsetree
  let parse_error loc msg = raise (ParseError (loc, msg))

  let dfl_ty ty = 
    match ty with 
    | None -> Expr.w1 
    | Some ty -> ty 
%}

%token PROC END PUBLIC INPUTS OUTPUTS SHARES RANDOMS OTHERS OPERATOR BIJ ARROW Sni
%token <string>SNI NI PROBING SPINI PRINT STRING

%token <string>READ_FILE READ_ILANG
%token BOOL W1 W8 W16 W32 W64 TINT
%token TRANSITION NOGLITCH PARA ORDER VERBOSE NOBOOL NOPRINT RESET
%token <string> IDENT OIDENT
%token <int> INT
%token <Z.t> LIT
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LCURLY
%token RCURLY
%token COMMA SEMICOLON COLON
%token MARK DOTEQ EQ LTDASH PIPEEQ
%token LEAK
%token LSR LSL
%token <string> ADD MUL NOT
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

oident:
  | x=loc(OIDENT) { x }

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

typ:
  | BOOL { Expr.w1  }
  | W1   { Expr.w1  } 
  | W8   { Expr.w8  }
  | W16  { Expr.w16 }
  | W32  { Expr.w32 }
  | W64  { Expr.w64 }
  | TINT { Expr.int }

id_range_decl:
  | ty=typ? x=ident r=range? { 
                        
    x, r, dfl_ty ty }

%inline vcall1:
  | x=id_range { Vid x }
  | LBRACKET xs=loc(separated_list(COMMA,ident)) RBRACKET { Vtuple xs }

%inline vcall:
  | x=vcall1 s=shift? { x, s }

expr_r:
  | x=vcall                    { Evar x }
  | LPAREN l=loc(LIT) COLON ty=typ RPAREN { Econst(l,ty) }                     
  | LPAREN e=expr RPAREN       { data e }
  | e1=expr o=loc(ADD) e2=expr { Eop(o,[e1; e2]) }
  | e1=expr o=loc(MUL) e2=expr { Eop(o,[e1; e2]) }
  | o=loc(NOT) e=expr          { Eop(o,[e]) }
  | o=oident LPAREN es=separated_list(COMMA,expr) RPAREN { Eop(o,es) } 

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
  | x=vcall LTDASH e=expr
    { {i_var = x; i_kind = IK_noleak; i_expr = e}}

%inline vcalls:
  | LPAREN xs=separated_list(COMMA,vcall) RPAREN { xs }

%inline lhs:
  | x=vcall { [x] }
  | LPAREN xs=separated_list(COMMA,vcall) RPAREN { xs }

call:
  | xs=lhs EQ f=ident ys=vcalls { {i_lhs = xs; i_macro = f; i_args = ys } }

%inline leak:
  | LEAK x=ident PIPEEQ LPAREN ls=separated_list(COMMA,expr) RPAREN
    { {l_name = x; l_exprs = ls} }
  | LEAK x=ident LPAREN ls=separated_list(COMMA,expr) RPAREN
    { {l_name = x; l_exprs = ls} }
  | x=ident PIPEEQ LPAREN ls=separated_list(COMMA,expr) RPAREN
    { {l_name = x; l_exprs = ls} }

instr:
  | l=leak msg=STRING? SEMICOLON  { 
      let msg = match msg with Some m -> m | _ -> "" in
      Ileak(l, msg) }
  | i=assgn SEMICOLON { Iassgn i }
  | i=call  SEMICOLON { Imacro i }

cmd:
  | c=list(loc(instr)) { c }

shares:
  | xs=separated_list (ADD, ident) {xs}

decl:
  | ty=typ? x=ident EQ xs=shares  { (x, Ids xs, dfl_ty ty) }
  | ty=typ? x=ident r=range { x, Range r, dfl_ty ty }

decls:
  | ds=separated_list(COMMA,decl) { ds }

inputs:
  | INPUTS COLON d=decls { d }
  | INPUTS SHARES COLON d=decls { d }

outputs:
  | OUTPUTS COLON d=decls { d }
  | OUTPUTS SHARES COLON d=decls { d }

shares_decl:
  |                      { [] }
  | SHARES COLON d=decls { d }
  | INPUTS SHARES COLON d=decls { d }

pub_inputs:
  |                                                     { [] }
  | PUBLIC INPUTS COLON d=separated_list(COMMA,id_range_decl) { d }

pub_outputs:
  |                                                     { [] }
  | PUBLIC OUTPUTS COLON d=separated_list(COMMA,  vcall1) { d }

randoms:
  |                                                     { [] }
  | RANDOMS COLON r=separated_list(COMMA,id_range_decl) { r }

other_var: 
  | ty=typ? x=ident { x, dfl_ty ty }
      
other_vars:
  |                                                 { [] }
  | OTHERS COLON r=separated_list(COMMA, other_var) { r }

func:
  | k=Sni? PROC f_name=ident COLON
      f_pin    = pub_inputs
      f_in     = inputs
      f_out    = outputs
      f_shares = shares_decl
      f_rand   = randoms
      f_other  = other_vars
      f_pout   = pub_outputs 
      SEMICOLON
      f_cmd    = cmd
    END
    { 
      let f_kind = match k with None -> NONE | Some _ -> SNI in
      { f_name; f_pin; f_in; f_out; f_pout; f_shares; f_rand; f_other; f_cmd; f_kind } }

sni_bound:
  | to_=INT          { 0,to_ }
  | from=INT COLON to_=INT { from, to_ }

check_opt:
  | ORDER n=INT { Order n }
  | PARA        { Para }
  | NOGLITCH    { NoGlitch }
  | NOBOOL      { NoBool }
  | NOPRINT     { NoPrint }
  | TRANSITION  { Transition }

bij_typ:
  | bij=BIJ? ty=typ { bij <> None, ty } 
ty_op:
  | dom=separated_list(COMMA, bij_typ) ARROW codom=typ { dom, codom }

command1:
  | bij=BIJ? OPERATOR o=oident COLON ty=ty_op { Operator(o, ty, bij <> None) }
  | f=func                               { Func f }
  | o=check_opt* f=loc(NI)               { NI (f,o) }
  | o=check_opt* b=sni_bound? f=loc(SNI) { SNI (f,b,o) }
  | o=check_opt* b=sni_bound? f=loc(SPINI) { SPINI (f,b,o) }
  | o=check_opt* f=loc(PROBING)          { Probing (f,o) }
  | f=loc(READ_FILE)                     { Read_file f  }
  | f=loc(READ_ILANG)                    { Read_ilang f }
  | f=loc(PRINT)                         { Print f }
  | RESET                                { Reset }
  | VERBOSE i=loc(INT)                   { Verbose i }
  | error             { parse_error (Location.make $startpos $endpos) None }

command:
  | c=command1     { c }
  | EOF            { Exit }

file:
  | c=list(command1) EOF { c }
