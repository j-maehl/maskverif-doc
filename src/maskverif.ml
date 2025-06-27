open Util
open Parsetree

(* Parsing utilities and wrappers *)
module Parse = struct

  module P = Parser
  module L = Lexing

  (* Create a lexbuf from a channel, setting file name and position *)
  let lexbuf_from_channel = fun name channel ->
    let lexbuf = Lexing.from_channel channel in
    lexbuf.Lexing.lex_curr_p <- {
        Lexing.pos_fname = name;
        Lexing.pos_lnum  = 1;
        Lexing.pos_bol   = 0;
        Lexing.pos_cnum  = 0
      };
    lexbuf

  (* Menhir parser entry points, adapted for revised API *)
  let parse_command = fun () ->
    MenhirLib.Convert.Simplified.traditional2revised Parser.command

  let parse_file = fun () ->
    MenhirLib.Convert.Simplified.traditional2revised Parser.file

  (* Lexer wrapper: returns token and positions *)
  let lexer lexbuf = fun () ->
    let token = Lexer.main lexbuf in
    (token, L.lexeme_start_p lexbuf, L.lexeme_end_p lexbuf)

  (* Parse from a channel, using a given parser *)
  let from_channel parse ~name channel =
    let lexbuf = lexbuf_from_channel name channel in
    parse () (lexer lexbuf)

  (* Parse from a file, ensuring the channel is closed *)
  let from_file parse filename =
    let channel = open_in filename in
    finally
      (fun () -> close_in channel)
      (from_channel parse ~name:filename) channel

  (* Parse a file as a list of commands *)
  let process_file filename =
    let decl = from_file parse_file filename in
    decl

  (* Lexbuf for stdin, used for interactive mode *)
  let stdbuf = lexbuf_from_channel "stdin" stdin

  (* Parse a command from stdin *)
  let process_command () =
    parse_command () (lexer stdbuf)

end

(* ----------------------------------------------------------- *)

let globals = Hashtbl.create 107
(* Global environment for storing functions *)

(* Options for the various checks *)
type check_opt = {
   trans     : bool;           (* Enable transition-based analysis *)
   glitch    : bool;           (* Enable glitch analysis *)
   para      : bool;           (* Enable parallelism *)
   order     : int option;     (* Order of security to check *)
   option    : Util.tool_opt;  (* Tool options *)
  }

(* Process a list of options into a check_opt record *)
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

(* Compute the order to use for a check, defaulting to nb_shares - 1 *)
let mk_order o nb_shares =
  match o.order with
  | None -> nb_shares - 1
  | Some i -> i

(* Pretty-print check options *)
let pp_option fmt o =
  Format.fprintf fmt "(%stransition,%sglitch)"
  (if o.trans then "" else "no ")
  (if o.glitch then "" else "no ")

(* Run a non-interference (NI) check on a function *)
let check_ni f o =
  let func = Prog.get_global globals f in
  Format.printf "Checking NI for %s: %a@." (data f) pp_option o;
  let (params, nb_shares, all, _) =
    Prog.build_obs_func ~trans:o.trans ~glitch:o.glitch ~ni:`NI (loc f) func in
(*  Format.printf "@[<v>observations:@ %a@]@." Checker.pp_eis all; *)
  let order = mk_order o nb_shares in
  Checker.check_ni ~para:o.para ~fname:(data f) o.option params ~order nb_shares all

(* Run a threshold probing check on a function *)
let check_threshold f o =
  let func = Prog.get_global globals f in
  let nb_shares =
    match func.Prog.f_in with
    | (_,xs) :: _ -> List.length xs
    | _           -> assert false in
  Format.printf "Checking Threshold for %s: %a@." (data f) pp_option o;
  let func = Prog.threshold func in
(*  Format.printf "%a@." (Prog.pp_func ~full:Prog.var_pinfo) func; *)
  let (params, _, all, _) =
    Prog.build_obs_func ~trans:o.trans ~glitch:o.glitch ~ni:`Threshold (loc f) func in
  let order = mk_order o nb_shares in
 (* Format.printf "@[<v>observations:@ %a@]@." Checker.pp_eis all; *)
  Checker.check_threshold o.option ~para:o.para order params all

(* Run a strong non-interference (SNI) check on a function *)
let check_sni f b o =
  let from, to_ =
    match b with None -> None, None | Some (i,j) -> Some i, Some j in
  let func = Prog.get_global globals f in
  Format.printf "Checking SNI for %s: %a@." (data f) pp_option o;
  let (params, nb_shares, interns, outputs) =
    Prog.build_obs_func ~trans:o.trans ~glitch:o.glitch ~ni:`SNI (loc f) func in
  let order = mk_order o nb_shares in
  Checker.check_sni o.option ~para:o.para ~fname:(data f) ?from ?to_ params nb_shares ~order interns outputs

(* Print a message when a function is added *)
let pp_added func =
  Format.printf "proc %a added@." (HS.pp false) func.Prog.f_name
(*  Format.printf "%a@." (Prog.pp_func ~full:Prog.dft_pinfo) func *)

(* Add a new operator to the environment *)
let add_operator o ty bij = 
  match List.rev ty with
  | [] -> assert false
  | ty::tys -> 
    try ignore (Expr.Op.find (data o)); error "" (Some(loc o)) "duplicate operator %s" (data o)
    with Not_found ->
    let o = Expr.Op.make (data o) (Some(List.rev tys, ty)) bij Expr.Other in
    Format.printf "operator %s added@." o.Expr.op_name

(* Process a single command from the AST *)
let rec process_command c =
  match c with
  | Operator (o, ty, bij) ->
    add_operator o ty bij
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
  | Verbose i -> Util.set_verbose (data i)
  | Exit ->
    Format.eprintf "Bye bye!@.";
    exit 0

(* Process all commands in a file *)
and process_file filename =
  let cs = Parse.process_file (data filename) in
  List.iter process_command cs

(* Main interactive loop *)
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
