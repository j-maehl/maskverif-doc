
(* -------------------------------------------------------------------- *)
type location = {
  lc_fname : string;
  lc_start : int * int;
  lc_end   : int * int;
  lc_bchar : int;
  lc_echar : int;
}

(* -------------------------------------------------------------------- *)
type 'a located = { pl_data: 'a; pl_location: location; }

let mkloc (lc : location) (x : 'a) =
  { pl_data = x; pl_location = lc; }

let loc  { pl_location = lc } = lc
let data { pl_data     = dt } = dt

(* -------------------------------------------------------------------- *)
module Location : sig
  open Lexing

  type t = location

  val make      : position -> position -> t
  val of_lexbuf : lexbuf -> t
  val to_string : t -> string
end = struct
  open Lexing

  type t = location

  let make (p1 : position) (p2 : position) =
    let mkpos (p : position) = (p.pos_lnum, p.pos_cnum - p.pos_bol) in
    { lc_fname = p1.pos_fname;
      lc_start = mkpos p1    ;
      lc_end   = mkpos p2    ;
      lc_bchar = p1.pos_cnum ;
      lc_echar = p2.pos_cnum ; }

  let of_lexbuf (lexbuf : lexbuf) =
    let p1 = Lexing.lexeme_start_p lexbuf in
    let p2 = Lexing.lexeme_end_p lexbuf in
    make p1 p2

  let to_string (lc : t) =
    let spos =
      if lc.lc_start = lc.lc_end then
        Printf.sprintf "line %d (%d)"
          (fst lc.lc_start) (snd lc.lc_start)
      else if fst lc.lc_start = fst lc.lc_end then
        Printf.sprintf "line %d (%d-%d)"
          (fst lc.lc_start) (snd lc.lc_start) (snd lc.lc_end)
      else
        Printf.sprintf "line %d (%d) to line %d (%d)"
          (fst lc.lc_start) (snd lc.lc_start)
          (fst lc.lc_end  ) (snd lc.lc_end  )
    in
      Printf.sprintf "%s: %s" lc.lc_fname spos
end

(* -------------------------------------------------------------------- *)
exception ParseError of location * string option

type ident = string located

type psharedvar = 
[ `Var   of ident
| `Share of ident * int ]

type pvar_decl = 
[ `Shared of ident 
| `Single of ident  ]

type pop = 
[ `Add | `Mul]

type pexpr = 
[ `Rnd 
| `Allias of psharedvar
| `Op     of psharedvar * pop * psharedvar ]

type pcommand = {
  lhs : psharedvar;
  rhs : pexpr
}

type return_expr = 
[ `Shared of ident
| `Tuple  of psharedvar list ]

type pkind =
[ `NI
| `SNI of int option ]
 
type pprog = { 
  p_name     : ident;
  p_params   : ident list;
  p_nbshares : int;
  p_decl     : pvar_decl list;
  p_body     : pcommand list;
  p_return   : return_expr located;
}







