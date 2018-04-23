{ 
  open Util
  open Ilang_parser

  let pp_lex_error fmt msg =
    Format.fprintf fmt "parse error: %s" msg

  let lex_error lexbuf msg =
    raise (LexicalError (Some (Location.of_lexbuf lexbuf), msg))

  let unterminated_comment () =
    raise (LexicalError (None, "unterminated comment"))

  let unterminated_string () =
    raise (LexicalError (None, "unterminated string"))

  let _keywords = [
    "autoidx"   , AUTOIDX;
    "attribute" , ATTRIBUTE;
    "module"    , MODULE;
    "end"       , END;
    "wire"      , WIRE; 
    "random"    , RANDOM;
    "public"    , PUBLIC;
    "input"     , INPUT;
    "output"    , OUTPUT;
    "width"     , WIDTH;
    "upto"      , UPTO;  
    "cell"      , CELL;
    "connect"   , CONNECT;
    "parameter" , PARAMETER;
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
let digit   = ['0'-'9']
let uint    = digit+
let other   = '.' | '[' | ']' | ':' | '$' | '\\' | '_'
let ident_start = '$' | '\\'
let ident = 
  ident_start (letter | digit | other)*  

let keywords = letter+ 

rule main = parse
  | newline     { Lexing.new_line lexbuf; main lexbuf }
  | blank+      { main lexbuf }
  | keywords as id { try Hashtbl.find keywords id 
                     with Not_found -> 
                       IDENT id }
  | ident as id  { IDENT id }
  | uint as n    { INT (int_of_string n) }
  | "\""         { STRING (Buffer.contents (string (Buffer.create 0) lexbuf)) } 
  | "##"         { SHARPSHARP }
  | "#"          { comment lexbuf; main lexbuf }

  | "["         { LBRACKET }
  | "]"         { RBRACKET }
  | "{"         { LCURLY }
  | "}"         { RCURLY }
  | ":"         { COLON }
  | "1'0" as c  { CONST c }
  | "1'1" as c  { CONST c }
  | eof         { EOF }

and comment = parse
  | newline     { Lexing.new_line lexbuf }
  | eof         { unterminated_comment () }
  | _           { comment lexbuf }


and string buf = parse
  | "\""          { buf }
  | "\\n"         { Buffer.add_char buf '\n'; string buf lexbuf }
  | "\\r"         { Buffer.add_char buf '\r'; string buf lexbuf }
  | "\\" (_ as c) { Buffer.add_char buf c   ; string buf lexbuf }
  | newline       { Buffer.add_string buf (Lexing.lexeme lexbuf);
                    Lexing.new_line lexbuf; string buf lexbuf }
  | eof           { unterminated_string () }
  | _ as c        { Buffer.add_char buf c   ; string buf lexbuf }

