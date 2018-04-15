open Util
open Parsetree

module Parse = struct 

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

  let parse_command = fun () ->
    MenhirLib.Convert.Simplified.traditional2revised Parser.command

  let parse_file = fun () ->
    MenhirLib.Convert.Simplified.traditional2revised Parser.file
    
  type parser_t =
    (P.token * L.position * L.position, unit list)
      MenhirLib.Convert.revised
    
  let lexer lexbuf = fun () ->
    let token = Lexer.main lexbuf in
    (token, L.lexeme_start_p lexbuf, L.lexeme_end_p lexbuf)
    
  let from_channel parse ~name channel =
    let lexbuf = lexbuf_from_channel name channel in
    parse () (lexer lexbuf)

  let from_file parse filename =
    let channel = open_in filename in
    finally
      (fun () -> close_in channel)
      (from_channel parse ~name:filename) channel
  
  let process_file filename = 
    let decl = from_file parse_file filename in
    decl

  let stdbuf = lexbuf_from_channel "stdin" stdin

  let process_command () =
    parse_command () (lexer stdbuf)

end
(* ----------------------------------------------------------- *)

open Checker
  
let globals = Hashtbl.create 107

let check_ni f = 
  let func = Prog.get_global globals f in
  Format.printf "Checking NI for %s:@." (data f);
  let (params, nb_shares, all, _) = 
    Prog.build_obs_func ~ni:`NI (loc f) func in
  Format.printf "@[<v>observations:@ %a@]@."
    (pp_list "@ " (fun fmt e -> Format.fprintf fmt "@[%a@]" Expr.pp_expr e))
    all;
  Checker.check_ni ~fname:(data f) params nb_shares all

let check_sni f =
  let func = Prog.get_global globals f in
  Format.printf "Checking SNI for %s:@." (data f);
  let (params, nb_shares, interns, outputs) = 
    Prog.build_obs_func ~ni:`SNI (loc f) func in
  Format.printf "@[<v>interns:@ %a@]@."
    (pp_list "@ " (fun fmt e -> Format.fprintf fmt "@[%a@]" Expr.pp_expr e))
    interns;
  Format.printf "@[<v>outputs:@ %a@]@."
    (pp_list "@ " (fun fmt e -> Format.fprintf fmt "@[%a@]" Expr.pp_expr e))
    outputs;  
  Checker.check_sni params nb_shares interns outputs 
  
let pp_added func = 
  Format.printf "proc %s added@." func.Prog.f_name.Expr.v_name 

let rec process_command c = 
  match c with
  | Func f ->
    pp_added (Prog.Process.func globals f)
  | NI f       -> check_ni f
  | SNI f      -> check_sni f
  | Probing _f -> assert false 
  | Read_file filename ->
    Format.eprintf "read_file %s@." (data filename);
    process_file filename
  | Read_ilang filename ->
    Format.eprintf "read_ilang %s@." (data filename);
    let func = Ilang.process_file (data filename) in
    Format.printf "%a@." (Prog.pp_func false) func;
    Prog.add_global globals func 
  | Exit -> 
    Format.eprintf "Bye bye!@.";
    exit 0

and process_file filename = 
  let cs = Parse.process_file (data filename) in
  List.iter process_command cs
  
let main =
  while true do 
    try 
      Format.printf ">"; Format.print_flush ();
      let c = Parse.process_command () in
      process_command c
    with 
      ParseError (l,s) -> 
      let s = match s with Some s -> s | None -> "" in
      Format.eprintf "Parse error at %s: %s@." (Location.to_string l) s;
      exit 1
    | Util.Error e ->
      Format.eprintf "%a@." Util.pp_error e
  done

