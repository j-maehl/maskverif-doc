open Util
open Ilang_ast

module Parse = struct 

  module P = Ilang_parser
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
    MenhirLib.Convert.Simplified.traditional2revised P.prog

  type parser_t =
    (P.token * L.position * L.position, unit list)
      MenhirLib.Convert.revised
    
  let lexer lexbuf = fun () ->
    let token = Ilang_lexer.main lexbuf in
    (token, L.lexeme_start_p lexbuf, L.lexeme_end_p lexbuf)

  let from_channel ~name channel =
    let lexbuf = lexbuf_from_channel name channel in
    parserprog () (lexer lexbuf)

  let from_file filename =
    let channel = open_in filename in
    finally
      (fun () -> close_in channel)
      (from_channel ~name:filename) channel
    
  let read_file filename = 
    let decl = from_file filename in
    decl

end

(* ----------------------------------------------------------- *)

type var = string * int option

type operator = 
  | Oand 
  | Onot
  | Oxor 
  | Off 
  | Oid

type arg = 
  | Avar of var
  | Aconst of string

type instr = {
    i_dest : var;
    i_op   : operator;
    i_args : arg list
  } 

type wire_decl = string * int 

type shares_by = int * int 

type ilang_module = {    
  ilm_name    : string;  
  ilm_randoms : wire_decl list;
  ilm_publics : wire_decl list;
  ilm_inputs  : (wire_decl * shares_by) list;
  ilm_outputs : (wire_decl * shares_by) list;
  ilm_others  : wire_decl list;
  ilm_cmd     : instr list;
  }

let pp_vident fmt i = 
  Format.fprintf fmt "%s" i

let pp_index fmt = function
  | None -> ()
  | Some i -> Format.fprintf fmt " [%i]" i

let pp_var fmt (x,i) = 
  Format.fprintf fmt "%a%a" pp_vident x pp_index i

let pp_arg fmt = function
  | Avar x -> pp_var fmt x 
  | Aconst x -> Format.fprintf fmt "%s" x 

let pp_instr fmt i = 
  match i.i_op, i.i_args with
  | Oid, [a1] ->
    Format.fprintf fmt "%a := %a" pp_var i.i_dest pp_arg a1
  | Onot, [a1] ->
    Format.fprintf fmt "%a := !%a;" pp_var i.i_dest pp_arg a1
  | Oand, [a1;a2] -> 
    Format.fprintf fmt "%a := %a * %a;"
      pp_var i.i_dest pp_arg a1 pp_arg a2
  | Oxor, [a1;a2] -> 
    Format.fprintf fmt "%a := %a + %a;" 
      pp_var i.i_dest pp_arg a1 pp_arg a2
  | Off, es ->
    Format.fprintf fmt "%a = #FF(@[%a@])" 
      pp_var i.i_dest (pp_list ",@ " pp_arg) es
  | _, _ -> assert false 
 
let pp_cmd fmt c = 
  Format.fprintf fmt "@[<v>%a@]" (pp_list "@ " pp_instr) c

let pp_module fmt m = 
   let pp_decls pp_decl s fmt = 
    Format.fprintf fmt "%s @[%a@]" s (pp_list ",@ " pp_decl) in
   let pp_decl fmt (x,i) = 
     if i = 1 then Format.fprintf fmt "%s" x 
     else Format.fprintf fmt "%s [%i]" x i in
   let pp_shared fmt ((x,i),(nb,nby)) = 
     Format.fprintf fmt "%s [%i] shares %i %i" x i nb nby in
   Format.fprintf fmt "@[<v>proc %s:@   @[<v>%a@ %a@ %a@ %a@ %a@ %a;@ @ @ %a@]@]"
     m.ilm_name 
     (pp_decls pp_decl   "publics:") m.ilm_publics
     (pp_decls pp_shared "inputs :") m.ilm_inputs
     (pp_decls pp_shared "outputs:") m.ilm_outputs
     (pp_decls pp_decl   "randoms:") m.ilm_randoms
     (pp_decls pp_decl   "others :") m.ilm_others
    pp_cmd m.ilm_cmd

(* ----------------------------------------------------------------------- *)

module Process = struct 

  type wire_kind = 
    | Random of int
    | Public of int 
    | Input  of (int * shares_by)
    | Output of (int * shares_by)
    | Other 

  type wire = { 
      w_kind  : wire_kind;   
      w_name  : string;
      w_width : int; 
    }
    
  let mk_shared w (i,sh) = 
    match sh with
    | None    -> i, (w,1)
    | Some (nb,iby) -> 
      if nb*iby <> w then error "ilang:" None "invalid share declaration";
      i,(nb,iby)

  let rec get_wkind w wo = 
    match wo with
    | [] -> Other
    | WO_random i :: _ -> Random i
    | WO_input  i :: _ -> Input  (mk_shared w i)
    | WO_output i :: _ -> Output (mk_shared w i)
    | WO_public i :: _ -> Public i
    | _ :: wo          -> get_wkind w wo
                        
  let rec get_wwidth wo = 
    match wo with
    | []              -> 1
    | WO_width i :: _ -> i
    | _ :: wo         -> get_wwidth wo
                       
  let process_wire (o,id) = 
    let w_width = get_wwidth o in
    {
      w_kind = get_wkind w_width o;
      w_name = data id;
      w_width;
    }
 
  module G = Graph.Pack.Digraph

  module Hv = Hashtbl.Make(G.V)

  type operator_desc = {
      o_input  : string list;
      o_output : string;
      o_desc   : operator;
    }

  let op_decl = [
      "$_NOT_", { o_input = ["\\A";]; o_output = "\\Y"; o_desc = Onot };
      "$_XOR_", { o_input = ["\\A";"\\B"]; o_output = "\\Y"; o_desc = Oxor };
      "$_AND_", { o_input = ["\\A";"\\B"]; o_output = "\\Y"; o_desc = Oand };
      "$_DFF_PP0_",
        { o_input = ["\\C";"\\D";"\\R";]; o_output = "\\Q"; o_desc = Off };
      "$_DFFSR_PPP_", 
        { o_input = ["\\C";"\\D";"\\R";"\\S"]; o_output = "\\Q"; o_desc = Off } 
    ]

  let op_tbl = 
    let tbl = Hashtbl.create 11 in
    List.iter (fun (s,od) -> Hashtbl.add tbl s od) op_decl;
    tbl
  
  let operator_desc s = 
    try Hashtbl.find op_tbl (data s)
    with Not_found -> 
      error "ilang:" (Some (loc s)) "unknown operator %s" (data s) 

  type vertex_kind = 
    | Vwire of var
    | Vinstr of instr

  type env = {
      vtbl : vertex_kind Hv.t;
      itbl : (var,G.V.t) Hashtbl.t;
      graph : G.t;
      mutable randoms : wire list;
      mutable publics : wire list;
      mutable inputs  : wire list;
      mutable outputs : wire list;
      mutable others  : wire list;
    }
           
  let empty_env () = {
      vtbl = Hv.create 1007;
      itbl = Hashtbl.create 1007;
      graph = G.create ~size:1007 ();
      randoms = [];
      publics = [];
      inputs  = [];
      outputs = [];
      others  = [];
    }

  let create_vertex env = 
    let v = G.V.create 0 in
    G.add_vertex env.graph v;
    v

  let add_wire env x = 
    let v = create_vertex env in
    Hv.add env.vtbl v (Vwire x);
    Hashtbl.add env.itbl x v

  let add_wires env x width = 
    if width = 1 then add_wire env (x, None)
    else 
      for i = 0 to width - 1 do 
        add_wire env (x, Some i) 
      done

  let add_random env w = 
    env.randoms <- w::env.randoms;
    add_wires env w.w_name w.w_width

  let add_public env w = 
    env.publics <- w::env.publics;
    add_wires env w.w_name w.w_width

  let add_input env w = 
    env.inputs <- w::env.inputs;
    add_wires env w.w_name w.w_width

  let add_output env w = 
    env.outputs <- w::env.outputs
    
  let add_other env w = 
    env.others <- w::env.others

  let mk_var (x,i) = data x, i 

  let mk_arg = function
    | Eid x    -> Avar (mk_var x)
    | Econst s -> Aconst s

  let mk_instr c = 
    let od = operator_desc c.cell_name1 in
    let process_arg s = 
      let c = List.find (fun c -> data c.c_connect_lhs = s) c.cell_connect in
      mk_arg c.c_connect_rhs in
    let i_args = List.map process_arg od.o_input in
    let i_dest =
      match process_arg od.o_output with
      | Avar (x,i) -> (x,i)
      | _ -> assert false in
    let i_op = od.o_desc in
    { i_dest; i_op; i_args }
  
  let add_instr env i = 
    let v = create_vertex env in
    Hv.add env.vtbl v (Vinstr i);
    Hashtbl.add env.itbl i.i_dest v
  
  let process_cell env c = add_instr env (mk_instr c)
  
  let process_connect env c = 
    let doit (x, e) = 
      add_instr env { i_dest = mk_var x; i_op = Oid; i_args = [mk_arg e] } in
    match c.connect_lhs, c.connect_rhs with
    | (x,r), Rvect es ->
      let rng = 
        match r with
        | Some (i,j) -> mk_range_i i j
        | None -> mk_range_i (List.length es - 1) 0 in
      let xs = List.map (fun i -> x, Some i) rng in
      let xes = List.map2 (fun x e -> x,e) xs es in
      List.iter doit xes
    | (x,None), Rexpr e ->
      doit ((x,None),e)    
    | _, _ -> assert false 

  let process_module m = 
    let env = empty_env () in
    (* initialize the graph *)
    let process_wire w =
      let w = process_wire w in
      match w.w_kind with 
      | Random _ -> add_random env w
      | Public _ -> add_public env w 
      | Input  _ -> add_input  env w
      | Output _ -> add_output env w 
      | Other    -> add_other  env w in
    let process_decl md = 
      match md.wa_data with 
      | Wire w    -> process_wire w
      | Cell c    -> process_cell env c
      | Connect c -> process_connect env c in
    List.iter process_decl m.mod_decl;
    (* add edges *) 
    let add_edge v2 = function
      | Vwire _ -> ()
      | Vinstr i ->
        let add_arg = function
          | Avar x ->
            let v1 = 
              try Hashtbl.find env.itbl x with Not_found -> assert false in
            G.add_edge env.graph v1 v2 
          | Aconst _ -> () in
        List.iter add_arg i.i_args in
    Hv.iter add_edge env.vtbl;
    (* Build the list of instruction *)
    let doit v c = 
      let k = 
        try Hv.find env.vtbl v with Not_found -> assert false in
      match k with
      | Vwire _ -> c
      | Vinstr i -> i::c in
    let c = G.Topological.fold doit env.graph [] in

    (* build the input *)
    let get_kind w = 
      match w.w_kind with
      | Public i | Input (i,_) | Output (i,_) | Random i -> i
      | Other -> 0 in
    let get_share w = 
      match w.w_kind with
      | Input (_,sh) | Output (_,sh) -> sh 
      | _ -> assert false in
    let sort = List.sort (fun w1 w2 -> get_kind w1 - get_kind w2) in
    let decls = List.map (fun w -> w.w_name, w.w_width) in
    let decls_shared = 
      List.map (fun w -> (w.w_name, w.w_width), get_share w) in 
    { 
      ilm_name    = data m.mod_name;  
      ilm_randoms = decls (sort env.randoms);
      ilm_publics = decls (sort env.publics);
      ilm_inputs  = decls_shared (sort env.inputs);
      ilm_outputs = decls_shared (sort env.outputs);
      ilm_others  = decls env.others; 
      ilm_cmd     = List.rev c;
    }

  let process_prog p = 
    process_module  p.module_decl.wa_data

end
(* ----------------------------------------------------------- *)

module ToProg = struct 
  module E=Expr
  module P = Prog

  type envm = {
      vtbl: (var, E.var * P.expr) Hashtbl.t;
    } 

  let empty_envm () = {
      vtbl = Hashtbl.create 107;
    }
                    
  let mk_op op = HS.make op
               
  let mk_var envm (s,i as x) = 
    let xn = 
      match i with 
      | None -> s
      | Some i -> Format.sprintf "%s.[%i]" s i in
    let v = E.V.mk_var xn in
    let e = P.Evar v in
    Hashtbl.add envm.vtbl x (v,e);
    v
    
  let mk_var_w envm (x, w) = 
    if w = 1 then [mk_var envm (x,None)]
    else
      List.map (fun i -> mk_var envm (x,Some i)) (mk_range_i 0 (w-1))

  let mk_vars envm decls = 
    List.flatten (List.map (mk_var_w envm) decls) 

  let nb_shares ((_,_),(nb,_)) = nb

  let mk_in envm ((x,sz),(nb,iby)) = 
    if sz = nb then
      [E.V.mk_var x, mk_var_w envm (x,sz)]
    else 
      let ivar = mk_range_i 0 (sz/nb - 1) in
      let ish  = mk_range_i 0 (nb-1) in
      let doit iv =
        let start = 
          if iby = 1 then iv * nb 
          else iv in
        E.V.mk_var x, 
        List.map (fun i -> mk_var envm (x, Some (start + i*iby))) ish in
      List.map doit ivar 
      
  let mk_inputs envm ins = 
    match ins with
    | w :: _ -> 
      let nbshares = nb_shares w in
      if not (List.for_all (fun w -> nbshares = nb_shares w) ins) then
        error "ilang_to_prog:" None "numbers of shares not equals";
      nbshares, List.flatten (List.map (mk_in envm) ins)
    | _ -> error "ilang_to_prog:" None "no input"
      

  let mk_outputs envm nbshares out = 
    if not (List.for_all (fun w -> nbshares = nb_shares w) out) then
      error "ilang_to_prog:" None "invalid numbers of output shares";
    List.map snd (List.flatten (List.map (mk_in envm) out))
  
  let to_var envm x = 
    try Hashtbl.find envm.vtbl x 
    with Not_found -> assert false 

  let ksubst = Parsetree.IK_subst 
  let kglitch = Parsetree.IK_glitch 

  let to_arg envm = function
    | Avar x -> snd (to_var envm x)
    | Aconst c -> P.Eop (mk_op c, [])

  let op_ff = mk_op "$FF"

  let to_expr envm op args = 
    match op, List.map (to_arg envm) args with
    | Oand, [e1; e2] -> ksubst , P.Emul(e1, e2)
    | Oxor, [e1; e2] -> ksubst , P.Eadd(e1, e2)
    | Onot, [e1]     -> ksubst , P.Enot e1
    | Off ,  es      -> kglitch, P.Eop(op_ff, es)
    | Oid , [e]      -> ksubst , e
    | _   , _        -> assert false 

  let mk_instr envm i = 
    let i_var = fst (to_var envm i.i_dest) in
    let i_kind, i_expr = to_expr envm i.i_op i.i_args in 
    P.Iassgn P.{ i_var; i_kind; i_expr}
    
  let func_of_mod modul = 
    let envm = empty_envm () in
    let f_name = Expr.V.mk_var modul.ilm_name in
    let f_pin = mk_vars envm modul.ilm_publics in
    let nshare, f_in = mk_inputs envm modul.ilm_inputs in
    let f_out   = mk_outputs envm nshare modul.ilm_outputs in
    let f_other = mk_vars envm modul.ilm_others  in
    let f_rand  = mk_vars envm modul.ilm_randoms in
    let f_cmd  = List.map (mk_instr envm) modul.ilm_cmd in
    P.{ f_name; f_pin; f_in; f_out; f_other; f_rand; f_cmd }


end

(* ----------------------------------------------------------- *)
    
let process_file filename =  
  let prog = Parse.read_file filename in
  let mod_ = Process.process_prog prog in
  let func = ToProg.func_of_mod mod_ in
  func  

