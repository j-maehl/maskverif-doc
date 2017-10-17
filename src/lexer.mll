{

  open Parser

  let _keywords = [
    "shared", SHARED;
    "var"   , VAR;  
    "ni"    , NI; 
    "sni"   , SNI;
    "return", RETURN ]

  let keywords =
    let table = Hashtbl.create 0 in
    List.iter (fun (s, c) -> Hashtbl.add table s c) _keywords; 
    table
}


let empty=""
let blank   = [' ' '\t' '\r']
let newline = '\n'

let upper   = ['A'-'Z']
let lower   = ['a'-'z']
let letter  = upper | lower
let digit   = ['0'-'9']
let uint    = digit+

let ident   = letter+ 
let underint = '_' uint

let pident = ident underint*

rule main = parse
  | newline      { Lexing.new_line lexbuf; main lexbuf }
  | blank+       { main lexbuf }
  | pident as id  { try Hashtbl.find keywords id with Not_found -> VIDENT id }
  | uint as i    { INT (int_of_string i) }
  | "$"          { DOLLAR    }
  | "="          { EQ        }
  | "+"          { ADD       }
  | "*"          { MUL       }
  | ";"          { SEMICOLON }
  | ","          { COMMA     }
  | "("          { LPAREN    }
  | ")"          { RPAREN    }
  | "{"          { LBRACKET  }
  | "}"          { RBRACKET  }

