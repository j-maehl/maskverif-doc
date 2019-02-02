{ 
  open Util
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
    "shares" , SHARES;
    "randoms", RANDOMS;
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
    "verbose", VERBOSE
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

let ident = char (char | digit | '\'')*

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
  | uint as n    { INT (int_of_string n) }
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
  | "+"         { ADD }
  | "*"         { MUL }
  | "~"         { NOT }
  | "!"         { MARK }
  | ";"         { SEMICOLON }
  | ">>"        { LSR }
  | "<<"        { LSL }
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

and comment = parse
  | "*)"        { () }
  | "(*"        { comment lexbuf; comment lexbuf }
  | newline     { Lexing.new_line lexbuf; comment lexbuf }
  | eof         { unterminated_comment () }
  | _           { comment lexbuf }


