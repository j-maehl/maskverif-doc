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

type check_opt = {
   glitch : bool;
   para   : bool;
   order  : int option;
  }

let process_check_opt os = 
  let glitch = ref true in
  let para = ref false in
  let order = ref None in
  let doit = function
    | NoGlitch -> glitch := false 
    | Para -> para := true
    | Order n -> order := Some n in
  List.iter doit os;
  { glitch = !glitch;
    para   = !para;
    order  = !order;
  }

let check_ni f o = 
  let func = Prog.get_global globals f in
  Format.printf "Checking NI for %s:@." (data f);
  let (params, nb_shares, all, _) = 
    Prog.build_obs_func ~glitch:o.glitch ~ni:`NI (loc f) func in
(*  Format.printf "@[<v>observations:@ %a@]@." Checker.pp_eis all; *)
  let order = 
    match o.order with
    | None -> nb_shares - 1
    | Some i -> i in
  Checker.check_ni ~para:o.para ~fname:(data f) params ~order nb_shares all

let check_threshold f o = 
  let func = Prog.get_global globals f in
  let order = 
    match func.Prog.f_in with
    | (_,xs) :: _ -> List.length xs - 1 
    | _           -> assert false in
  assert (0 < order);
  Format.printf "Checking Threshold for %s:@." (data f);
  let func = Prog.threshold func in
(*  Format.printf "%a@." (Prog.pp_func ~full:Prog.var_pinfo) func; *)
  let (params, _, all, _) = 
    Prog.build_obs_func ~glitch:o.glitch ~ni:`Threshold (loc f) func in
 (* Format.printf "@[<v>observations:@ %a@]@." Checker.pp_eis all; *)
  Checker.check_threshold ~para:o.para order params all
             
let check_sni f b o =
  let from, to_ = 
    match b with None -> None, None | Some (i,j) -> Some i, Some j in
  let func = Prog.get_global globals f in
  Format.printf "Checking SNI for %s:@." (data f);
  let (params, nb_shares, interns, outputs) = 
    Prog.build_obs_func ~glitch:o.glitch ~ni:`SNI (loc f) func in
(*    Format.printf "@[<v>interns:@ %a@]@." Checker.pp_eis interns;
    Format.printf "@[<v>outputs:@ %a@]@." Checker.pp_eis outputs;  *)
  Checker.check_sni ~para:o.para ~fname:(data f) ?from ?to_ params nb_shares interns outputs 
  
let pp_added func = 
  Format.printf "proc %s added@." func.Prog.f_name.Expr.v_name
(*  Format.printf "%a@." (Prog.pp_func ~full:Prog.dft_pinfo) func *)


let rec process_command c = 
  match c with
  | Func f ->
    pp_added (Prog.Process.func globals f)
  | NI (f,o)     -> check_ni f (process_check_opt o)
  | SNI (f,b,o)  -> check_sni f b (process_check_opt o)
  | Probing(f,o) -> check_threshold f (process_check_opt o)
  | Read_file filename ->
    Format.eprintf "read_file %s@." (data filename);
    process_file filename
  | Read_ilang filename ->
    Format.eprintf "read_ilang %s@." (data filename);
    let func = Ilang.process_file (data filename) in
    Prog.add_global globals func 
  | Print f -> 
    let func = Prog.get_global globals f in
    Format.printf "%a@." (Prog.pp_func ~full:Prog.dft_pinfo) func
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

