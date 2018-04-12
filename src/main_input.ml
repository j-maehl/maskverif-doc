open Util
open Parsetree

module P = Parser
module L = Lexing


let lexbuf_from_channel = fun name channel ->
  let lexbuf = Lexing.from_channel channel in
  lexbuf.Lexing.lex_curr_p <- {
    Lexing.pos_fname = name;
    Lexing.pos_lnum  = 1;
    Lexing.pos_bol   = 0;
    Lexing.pos_cnum  = 0
  };
  lexbuf


let parserprog = fun () ->
  MenhirLib.Convert.Simplified.traditional2revised Parser.prog

type parser_t =
  (P.token * L.position * L.position, unit list)
    MenhirLib.Convert.revised
    
let lexer lexbuf = fun () ->
  let token = Lexer.main lexbuf in
  (token, L.lexeme_start_p lexbuf, L.lexeme_end_p lexbuf)

let from_channel ~name channel =
  let lexbuf = lexbuf_from_channel name channel in
  parserprog () (lexer lexbuf)

let from_file filename =
  let channel = open_in filename in
  finally
    (fun () -> close_in channel)
    (from_channel ~name:filename) channel
  
let process_file filename = 
  let decl = from_file filename in
  decl

(* ----------------------------------------------------------- *)

open Checker
    
let main =  
  let filename = 
    if Array.length Sys.argv <= 1 then begin 
      Format.eprintf "usage: main_input filename@.";
      exit 1
    end;
    Sys.argv.(1) in
  let prog = 
    try process_file filename 
    with ParseError (l,s) -> 
      let s = match s with Some s -> s | None -> "" in
      Format.eprintf "Parse error at %s: %s@." (Location.to_string l) s;
      exit 1
  in
  Format.printf "%a@." pp_prog prog;
  try 
    let prog = Prog.to_prog prog in
    Format.printf "@.@.After parsing@.";
    Format.printf "%a@." (Prog.pp_prog false) prog;

    let prog = Prog.macro_expand_prog prog in
    Format.printf "@.@.After expand@.";
    Format.printf "%a@." (Prog.pp_prog false) prog;

    let prog = Prog.simplify_prog prog in
    Format.printf "@.@.After simplification@.";
    Format.printf "%a@." (Prog.pp_prog true) prog;

    let f = List.hd (List.rev prog) in
    let (params, nb_shares, interns, outs) = Prog.build_obs_func f in
    let all = interns @ outs in
    Format.printf "@[<v>observations:@ %a@]@."
      (pp_list "@ " (fun fmt e -> Format.fprintf fmt "@[%a@]" Expr.pp_expr e))
      all;
    Checker.check_ni params nb_shares all; 
    exit 0
  with Util.Error e ->
    Format.eprintf "%a@." Util.pp_error e;    
    exit 1


