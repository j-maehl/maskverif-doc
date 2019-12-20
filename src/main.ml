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
   trans     : bool;
   glitch    : bool;
   para      : bool;
   order     : int option;
   option    : Util.tool_opt;
  }

let process_check_opt os =
  let trans = ref false in
  let glitch = ref true in
  let para = ref false in
  let order = ref None in
  let bool = ref true in
  let print = ref true in
  let doit = function
    | Transition -> trans := true
    | NoGlitch -> glitch := false
    | Para -> para := true
    | Order n -> order := Some n
    | NoBool -> bool := false
    | NoPrint -> print := false in
  List.iter doit os;
  { trans  = !trans;
    glitch = !glitch;
    para   = !para;
    order  = !order;
    option = { pp_error = !print; checkbool = !bool; };
  }

let mk_order o nb_shares =
  match o with
  | None -> nb_shares - 1
  | Some i -> i

let pp_option fmt o =
  Format.fprintf fmt "(%stransition,%sglitch)"
  (if o.trans then "" else "no ")
  (if o.glitch then "" else "no ")

let check_ni f o =
  let func = Prog.get_global globals f in
  Format.printf "Checking NI for %s: %a@." (data f) pp_option o;
  let (params, nb_shares, all, _, _) =
    Prog.build_obs_func ~trans:o.trans ~glitch:o.glitch ~ni:`NI (loc f) func in
(*  Format.printf "@[<v>observations:@ %a@]@." Checker.pp_eis all; *)
  let order = mk_order o.order nb_shares in
  Checker.check_ni ~para:o.para ~fname:(data f) o.option params ~order nb_shares all

let check_threshold f o =
  let func = Prog.get_global globals f in
  let nb_shares =
    match func.Prog.f_in with
    | (_,xs) :: _ -> List.length xs
    | _           -> assert false in
  Format.printf "Checking Threshold for %s: %a@." (data f) pp_option o;
  let func = Prog.threshold func in
(*  Format.printf "%a@." (Prog.pp_func ~full:Prog.var_pinfo) func; *)
  let (params, _, all, _, _) =
    Prog.build_obs_func ~trans:o.trans ~glitch:o.glitch ~ni:`Threshold (loc f) func in
  let order = mk_order o.order nb_shares in
 (* Format.printf "@[<v>observations:@ %a@]@." Checker.pp_eis all; *)
  Checker.check_threshold o.option ~para:o.para order params all

let check_sni f b o =
  let from, to_ =
    match b with None -> None, None | Some (i,j) -> Some i, Some j in
  let func = Prog.get_global globals f in
  Format.printf "Checking SNI for %s: %a@." (data f) pp_option o;
  let (params, nb_shares, interns, outputs, _) =
    Prog.build_obs_func ~trans:o.trans ~glitch:o.glitch ~ni:`SNI (loc f) func in
  let order = mk_order o.order nb_shares in
  Checker.check_sni o.option ~para:o.para ~fname:(data f) ?from ?to_ params nb_shares ~order interns outputs 
 
let check_spini f b o =
  let from, to_ = 
    match b with None -> None, None | Some (i,j) -> Some i, Some j in
  let func = Prog.get_global globals f in
  Format.printf "Checking SPINI for %s: %a@." (data f) pp_option o;
  let (params, nb_shares, interns, outputs, _) = 
    Prog.build_obs_func ~trans:o.trans ~glitch:o.glitch ~ni:`SNI (loc f) func in
  let order = mk_order o.order nb_shares in
  Checker.check_spini o.option ~para:o.para ~fname:(data f) ?from ?to_ params nb_shares ~order interns outputs 

let pp_added func = 
  Format.printf "proc %s added@." func.Prog.f_name.hs_str

let add_operator o (dom, codom) bij = 
  let bij_a = Array.of_list (List.map fst dom) in
  let dom   = List.map snd dom in 
  let bij   =
    if Array.exists (fun b -> b) bij_a then Expr.PartialBij bij_a
    else
    if bij then Expr.Bij
    else Expr.NotBij in
  try ignore (Expr.Op.find (data o)); error "" (Some(loc o)) "duplicate operator %s" (data o)
  with Not_found ->
    let o = Expr.Op.make (data o) (Some(dom, codom)) bij Expr.Other in
    Format.printf "operator %s added@." o.Expr.op_name
   
let add_func f = 
  let func = Prog.Process.func globals f in
  pp_added func;
  if func.f_kind = Util.SNI then
    let (params, nb_shares, interns, outputs, rndo) =
      (* FIXME trans and glitch *)
      Prog.build_obs_func ~trans:false ~glitch:true ~ni:`RSNI (loc f.f_name) func in
    let order = mk_order None nb_shares in
    let o = { pp_error = true; checkbool = false; } in
(*    Checker.check_sni o 
                ~para:true ~fname:(data f.f_name) ?from:None ?to_:None params nb_shares ~order interns outputs; *)
    ignore (Checker.check_rndo o ~para:true ~fname:(data f.f_name) params
      nb_shares ~order rndo)
    
    

let rec process_command c =
  match c with
  | Operator (o, ty, bij) ->
    add_operator o ty bij
  | Func f ->
    add_func f
  | NI (f,o)     -> ignore(check_ni f (process_check_opt o))
  | SNI (f,b,o)  -> ignore(check_sni f b (process_check_opt o))
  | SPINI(f,b,o) -> ignore(check_spini f b (process_check_opt o))
  | Probing(f,o) -> ignore(check_threshold f (process_check_opt o))
  | Read_file filename ->
    Format.eprintf "read_file %s@." (data filename);
    process_file filename
  | Read_ilang filename ->
    Format.eprintf "read_ilang %s@." (data filename);
    let func = Ilang.process_file (data filename) in
    Prog.add_global globals func
  | Print f ->
    let func = Prog.get_global globals f in
    Format.printf "%a@." (Prog.pp_func ~full:Prog.var_pinfo) func
  | Verbose i -> Util.set_verbose (data i)
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
    | ParseError (l,s) ->
      let s = match s with Some s -> s | None -> "" in
      Format.eprintf "Parse error at %s: %s@." (Location.to_string l) s;
      exit 1
    | LexicalError (l,s) ->
      let pp_loc fmt loc =
        match loc with
        | None -> ()
        | Some loc -> Format.fprintf fmt " at %s" (Location.to_string loc) in
      Format.eprintf "Lexical error %a : %s@." pp_loc l s

    | Util.Error e ->
      Format.eprintf "%a@." Util.pp_error e
  done
