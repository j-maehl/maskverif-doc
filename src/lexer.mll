{ 
  open Util
  open Parser
  open Parsetree 

  exception LexicalError of Location.t option * string

  let pp_lex_error fmt msg =
    Format.fprintf fmt "parse error: %s" msg

  let lex_error lexbuf msg =
    raise (LexicalError (Some (Location.of_lexbuf lexbuf), msg))

  let unterminated_comment () =
    raise (LexicalError (None, "unterminated comment"))

  let _keywords = [
    "proc"   , PROC;
    "inputs" , INPUTS;
    "outputs", OUTPUTS;
    "randoms", RANDOMS;
    "shares" , SHARES
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
  | ident as id { try Hashtbl.find keywords id with Not_found -> IDENT id }
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
  | "!"         { NOT }
  | ";"         { SEMICOLON }
  | eof         { EOF }

and comment = parse
  | "*)"        { () }
  | "(*"        { comment lexbuf; comment lexbuf }
  | newline     { Lexing.new_line lexbuf; comment lexbuf }
  | eof         { unterminated_comment () }
  | _           { comment lexbuf }


