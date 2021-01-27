{
  open Maskverif.Util
  open Parser


  let pp_lex_error fmt msg =
    Format.fprintf fmt "parse error: %s" msg

  let lex_error lexbuf msg =
    raise (LexicalError (Some (Location.of_lexbuf lexbuf), msg))

  let unterminated_comment () =
    raise (LexicalError (None, "unterminated comment"))

  let unterminated_string () =
    raise (LexicalError (None, "unterminated string"))

  let _keywords = [
    "proc"   , PROC;
    "end"    , END;
    "public" , PUBLIC;
    "inputs" , INPUTS;
    "outputs", OUTPUTS;
    "input"  , INPUTS;
    "output" , OUTPUTS;
    "others" , OTHERS; 
    "op"     , OPERATOR;
    "bij"    , BIJ;
    "shares" , SHARES;
    "randoms", RANDOMS;
    "leak"   , LEAK;
    "NI"     , NI "";
    "SNI"    , SNI "";
    "Probing", PROBING "";
    "print", PRINT "";
    "read_file" , READ_FILE "";
    "read_ilang", READ_ILANG "";
    "para", PARA;
    "noglitch", NOGLITCH;
    "transition", TRANSITION;
    "nobool", NOBOOL;
    "noprint", NOPRINT;
    "order", ORDER;
    "verbose", VERBOSE;
    "bool", BOOL;
    "w1", W1;
    "w8", W8;
    "w16", W16;
    "w32", W32;
    "w64", W64;

  ]

  let keywords =
    let table = Hashtbl.create 0 in
    List.iter (fun (x,y) -> Hashtbl.add table x y) _keywords;
    table

}

let empty = ""
let blank = [' ' '\t' '\r' ]

let newline = '\n'
let upper   = ['A'-'Z']
let lower   = ['a'-'z']
let letter  = upper | lower
let char    = letter | '_'
let digit   = ['0'-'9']
let uint    = digit+
let int     = "-"? digit+

let litb = '0' 'b' ('0' | '1')+
let litx = '0' 'x' (digit | ['A'-'F'])+

  
let ident = char (char | digit | '\'')*
let sharpident = '#' ident 

rule main = parse
  | newline     { Lexing.new_line lexbuf; main lexbuf }
  | blank+      { main lexbuf }
  | ident as id {
                 let token =
                   try Hashtbl.find keywords id with Not_found -> IDENT id in
                 let to_string lexbuf =
                   Buffer.contents (read_blank_string (Buffer.create 0) lexbuf)
                 in
                 match token with
                 | NI _         -> NI (to_string lexbuf)
                 | SNI _        -> SNI (to_string lexbuf)
                 | PROBING _    -> PROBING (to_string lexbuf)
                 | PRINT _      -> PRINT (to_string lexbuf)
                 | READ_FILE _  -> READ_FILE (to_string lexbuf)
                 | READ_ILANG _ -> READ_ILANG (to_string lexbuf)
                 | _ -> token
               }
  | sharpident as id { OIDENT  id }
  | int as n     { INT (int_of_string n) }
  | litb as n    { LIT (Z.of_string n) }
  | litx as n    { LIT (Z.of_string n) }
  | "0d" (uint as n) { LIT (Z.of_string n) }  
  | "(*"        { comment lexbuf; main lexbuf }

  | "("         { LPAREN }
  | ")"         { RPAREN }
  | "["         { LBRACKET }
  | "]"         { RBRACKET }
  | "{"         { LCURLY }
  | "}"         { RCURLY }
  | ","         { COMMA }
  | ":"         { COLON }
  | "="         { EQ }
  | ":="        { DOTEQ }
  | "<-"        { LTDASH }
  | "|="        { PIPEEQ }

  | "+"         { ADD "^" }
  | "^w1"       { ADD "^" }
  | "^w8"       { ADD "^w8" }
  | "^w16"      { ADD "^w16" }
  | "^w32"      { ADD "^w32" }
  | "^w64"      { ADD "^w64"}

  | "*"         { MUL "&" }
  | "&w1"       { MUL "&" }
  | "&w8"       { MUL "&w8" }
  | "&w16"      { MUL "&w16" }
  | "&w32"      { MUL "&w32" }
  | "&w64"      { MUL "&w64"}

  | "~"         { NOT "~" }
  | "~w1"       { NOT "~" }
  | "~w8"       { NOT "~w8" }
  | "~w16"      { NOT "~w16" }
  | "~w32"      { NOT "~w32" }
  | "~w64"      { NOT "~w64"}

  | "!"         { MARK }
  | ";"         { SEMICOLON }
  | ">>"        { LSR }
  | "<<"        { LSL }
  | "->"        { ARROW }
  | "\""        { STRING (Buffer.contents (string (Buffer.create 0) lexbuf)) }
  | eof         { EOF }
  |  _         as c { lex_error lexbuf (Printf.sprintf "illegal character: %c" c) }

and read_blank_string buf = parse
  | blank       { read_blank_string buf lexbuf }
  | newline     { Lexing.new_line lexbuf; read_blank_string buf lexbuf }
  | eof         { unterminated_string () }
  | _ as c      { Buffer.add_char buf c;read_string buf lexbuf }

and read_string buf = parse
  | blank         { buf }
  | newline       { Lexing.new_line lexbuf; buf }
  | eof           { buf }
  | _ as c        { Buffer.add_char buf c; read_string buf lexbuf }

and string buf = parse
  | "\""          { buf }
  | "\\n"         { Buffer.add_char buf '\n'; string buf lexbuf }
  | "\\r"         { Buffer.add_char buf '\r'; string buf lexbuf }
  | "\\" (_ as c) { Buffer.add_char buf c   ; string buf lexbuf }
  | newline       { Buffer.add_string buf (Lexing.lexeme lexbuf); string buf lexbuf }
  | _ as c        { Buffer.add_char buf c   ; string buf lexbuf }
  | eof           { unterminated_string () }

and comment = parse
  | "*)"        { () }
  | "(*"        { comment lexbuf; comment lexbuf }
  | newline     { Lexing.new_line lexbuf; comment lexbuf }
  | eof         { unterminated_comment () }
  | _           { comment lexbuf }
