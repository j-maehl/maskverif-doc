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
  | Off of Util.HS.t
  | Oid

type arg = 
  | Avar of var
  | Aconst of string

type instr = {
    i_dest : var;
    i_op   : operator;
    i_args : arg list;
    i_attribute : attribute list * Location.t
  } 

type ilang_module = {    
  ilm_name     : string;  
  ilm_randoms  : var list;
  ilm_publics  : var list;
  ilm_inputs   : (string option * var list) list;
  ilm_outputs  : (var list) list;
  ilm_others   : var list;
  ilm_cmd      : instr list;
  }

(* ----------------------------------------------------------- *)

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
    Format.fprintf fmt "%a = %a" pp_var i.i_dest pp_arg a1
  | Onot, [a1] ->
    Format.fprintf fmt "%a = ~%a;" pp_var i.i_dest pp_arg a1
  | Oand, [a1;a2] -> 
    Format.fprintf fmt "%a = %a * %a;"
      pp_var i.i_dest pp_arg a1 pp_arg a2
  | Oxor, [a1;a2] -> 
    Format.fprintf fmt "%a = %a + %a;" 
      pp_var i.i_dest pp_arg a1 pp_arg a2
  | Off s, es ->
    Format.fprintf fmt "%a = %s(@[%a@])" 
      pp_var i.i_dest s.hs_str (pp_list ",@ " pp_arg) es
  | _, _ -> assert false 
 
(* ----------------------------------------------------------------------- *)

module Process = struct 

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
        { o_input = ["\\C";"\\D";"\\R";]; o_output = "\\Q"; 
          o_desc = Off _DFF_PP0_};
      "$_DFF_PN0_",
        { o_input = ["\\C";"\\D";"\\R";]; o_output = "\\Q"; 
          o_desc = Off _DFF_PN0_};
    
      "$_DFFSR_PPP_", 
        { o_input = ["\\C";"\\D";"\\R";"\\S"]; o_output = "\\Q"; 
          o_desc = Off _DFFSR_PPP_} 
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

  type dir = Downto | Upto

  type env = {
      vtbl : vertex_kind Hv.t;
      itbl : (var,G.V.t) Hashtbl.t;
      graph : G.t;
      wire  : (string, int * dir) Hashtbl.t;
      mutable winputs  : string list;
      mutable woutputs : string list;
      mutable randoms: var list;
      mutable publics: var list;
      mutable inputs : (string option * var list) list;
      mutable outputs: (var list) list;
      mutable others : var list;
    }

  let empty_env () = {
      vtbl = Hv.create 1007;
      itbl = Hashtbl.create 1007;
      graph = G.create ~size:1007 ();
      wire = Hashtbl.create 1007;
      winputs  = [];
      woutputs = [];
      randoms = [];
      publics = [];
      inputs  = [];
      outputs = [];
      others  = [];
    }

  type wire_kind = 
    | Kinput  
    | Koutput 
    | Kother 

  let rec get_kind wo = 
    match wo with
    | [] -> Kother
    | WO_input  _ :: _ -> Kinput  
    | WO_output _ :: _ -> Koutput 
    | _ :: wo          -> get_kind wo 

  let rec get_width wo = 
    match wo with
    | []              -> 1
    | WO_width i :: _ -> i
    | _ :: wo         -> get_width wo

  let rec get_dir wo = 
    match wo with
    | [] -> Downto
    | WO_upto :: _ -> Upto
    | _ :: wo -> get_dir wo

  let create_vertex env = 
    let v = G.V.create 0 in
    G.add_vertex env.graph v;
    v

  let add_input env x = 
    let v = create_vertex env in
    Hv.add env.vtbl v (Vwire x);
    Hashtbl.add env.itbl x v

  let process_wire env md = 
    match data md.wa_data with
    | Wire (wo, x) ->
      let width = get_width wo in
      let dir   = get_dir wo in
      let s = data x in
      Hashtbl.add env.wire s (width, dir); 
      begin match get_kind wo with
      | Kinput  -> 
        if width = 1 then add_input env (s, None)
        else
          for i = 0 to width-1 do
            add_input env (s, Some i)
          done;
        env.winputs  <- s :: env.winputs
      | Koutput -> env.woutputs <- s :: env.woutputs
      | Kother -> 
        if width = 1 then 
          env.others <- (s, None) :: env.others
        else
          for i = 0 to width-1 do
            env.others <- (s, Some i) :: env.others
          done
      end
    | _ -> ()

  let ilang_error loc = error "ilang" (Some loc)

   let getw_width env x = 
     try Hashtbl.find env.wire (data x) 
     with Not_found -> ilang_error (loc x) "undeclared wire %s" (data x)

   let mk_var env (x,i) =
     let width,_dir = getw_width env x in
     let i = 
       match i with
       | None -> 
         if width <> 1 then 
           ilang_error (loc x) "%s has size %i" (data x) width;
         None
       | Some i -> 
         if not (0 <= i && i < width) then
           ilang_error (loc x) "invalid index for %s" (data x);
         if width = 1 then None
         else Some i in
     (data x, i)
     
   let mk_dir_range dir width = 
     if dir = Upto then mk_range_i 0 (width-1)
     else mk_range_i (width-1) 0

   let mk_shares env = function
     | Sident x -> 
       let width,dir = getw_width env x in
       let s = data x in
       let range = mk_dir_range dir width in
       List.map (fun i -> s, Some i) range 
       
     | Sindex (x,i,j) ->
       let width,_dir = getw_width env x in
       let s = data x in
       if not (0 <= i && i < width && 0 <= j && j < width) then
         ilang_error (loc x) "invalid range for %s" s;
       List.map (fun i -> s, Some i) (mk_range_i i j)
     | Svindex (x, ns) ->
       List.map (fun i -> mk_var env (x,Some i)) ns
     | Svect xs -> 
       List.map (mk_var env) xs

   let mk_param x sh = 
     match x with
     | Some x -> Some (data x)
     | None ->
       match sh with
       | Sident x -> Some (data x)
       | _        -> None 
     
   let process_random env (x,i) =
     let s = data x in 
     if i = None then 
       let width,_dir = getw_width env x in
       if width = 1 then
         env.randoms <- (s, None) :: env.randoms
       else
         for i = 0 to width-1 do 
           env.randoms <- (s, Some i) :: env.randoms
         done
     else env.randoms <- mk_var env (x,i) :: env.randoms

   let process_public env (x,i) =
     let s = data x in 
     if i = None then 
       let width,_dir = getw_width env x in
       if width = 1 then
         env.publics <- (s, None) :: env.publics
       else
         for i = 0 to width-1 do 
           env.publics <- (s, Some i) :: env.publics
         done
     else env.publics <- mk_var env (x,i) :: env.publics

   let process_decl env = function
     | Input (x,sh) ->
       env.inputs <- (mk_param x sh, mk_shares env sh) :: env.inputs
     | Output sh ->
       env.outputs <- mk_shares env sh :: env.outputs
     | Random xs ->
       List.iter (process_random env) xs
     | Public xs ->
       List.iter (process_public env) xs 
     
  let mk_arg env = function
    | Eid x    -> Avar (mk_var env x)
    | Econst s -> Aconst (data s)

  let mk_instr env c i_attribute =
    let od = operator_desc c.cell_name1 in
    let process_arg s = 
      let c = List.find (fun c -> data c.c_connect_lhs = s) c.cell_connect in
      mk_arg env c.c_connect_rhs in
    let i_args = List.map process_arg od.o_input in
    let i_dest =
      match process_arg od.o_output with
      | Avar (x,i) -> (x,i)
      | _ -> assert false in
    let i_op = od.o_desc in
    { i_dest; i_op; i_args; i_attribute }
  
  let add_instr env i = 
    let v = create_vertex env in
    Hv.add env.vtbl v (Vinstr i);
    Hashtbl.add env.itbl i.i_dest v
  
  let process_cell env c aa = add_instr env (mk_instr env c aa)
  
  let process_connect env c i_attribute = 
    let cloc = loc c in
    let c = data c in
    let do_v x = 
      let s = data x in
      let w,dir = getw_width env x in
      if w = 1 then [s ,None]
      else 
        List.map (fun i -> s, Some i) (mk_dir_range dir w) in

    let xs = 
      let mk_x = function
        | Eid (x,None) -> do_v x
        | Eid x ->  [mk_var env x]
        | Econst c -> ilang_error (loc c) "constant can not be used in a lhs" in
      match c.connect_lhs with
      | Rexpr e -> mk_x e 
      | Rvect es -> List.flatten (List.map mk_x es) in
         
    let es =
      let mk_e = function
        | Eid (x,None) -> List.map (fun x -> Avar x) (do_v x)
        | e -> [mk_arg env e] in
      match c.connect_rhs with
      | Rexpr e -> mk_e e 
      | Rvect es -> List.flatten (List.map mk_e es) in
    
    let n1 = List.length xs in
    let n2 = List.length es in
    if n1 <> n2 then
      ilang_error cloc "invalid size for connect (lhs:%i, rhs: %i)" n1 n2;
    let doi x e = 
      add_instr env { i_dest = x; i_op = Oid; i_args = [e]; i_attribute} in
    List.iter2 doi xs es

  let process_module m = 
    let env = empty_env () in
    (* initialize the wire *)
    List.iter (process_wire env) m.mod_decl;
    (* process the other declarations, and build the graph *)
    let process_decl md = 
      match data (md.wa_data) with 
      | Parameter _ -> ()
      | Wire _    -> () 
      | Cell c    -> process_cell env c (md.attribute, loc md.wa_data)
      | Connect c -> process_connect env c (md.attribute, loc md.wa_data)
      | Decl d    -> process_decl env d in
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

    (* Check that all input/output wire has been correctly 
       dispached to public/random/input *)
    let tbl = Hashtbl.create 100 in
    let add_in_out x = 
      let w,_dir = Hashtbl.find env.wire x in
      Hashtbl.add tbl x (Array.make w false) in
    List.iter add_in_out env.winputs;
    List.iter add_in_out env.woutputs;
    let set_var (x,i as v) = 
      let n = match i with None -> 0 | Some i -> i in
      let t = 
        try Hashtbl.find tbl x 
        with Not_found -> error "ilang" None "unknown %a" pp_var v in
      if t.(n) then error "ilang" None "multiple declaration of %a" pp_var v;
      t.(n) <- true in
    let set_in (_,vs) = List.iter set_var vs in
    let set_out vs = List.iter set_var vs in
    List.iter set_in env.inputs;
    List.iter set_out env.outputs;
    List.iter set_var env.randoms;
    List.iter set_var env.publics;
    let check x t = 
      Array.iteri (fun i b -> 
          if not b then
            error "ilang" None "the index %i of wire %s is not dispached" i x) t
    in
    Hashtbl.iter check tbl;
    
    (* build the ilang representation *)
    { 
      ilm_name    = data m.mod_name;  
      ilm_randoms = env.randoms;
      ilm_publics = env.publics;
      ilm_inputs  = env.inputs;
      ilm_outputs = env.outputs; 
      ilm_others  = env.others;
      ilm_cmd     = List.rev c;
    }

  let process_prog p = 
    process_module p.module_decl.wa_data

end
(* ----------------------------------------------------------- *)

module ToProg = struct 
  module E=Expr
  module P = Prog

  type envm = (var, E.var * P.expr) Hashtbl.t

  let empty_envm () = Hashtbl.create 107
                    
  let mk_op op = HS.make op
               
  let mk_var envm (s,i as x) = 
    let xn = 
      match i with 
      | None -> s
      | Some i -> Format.sprintf "%s.[%i]" s i in
    let v = E.V.mk_var xn in
    let e = P.Evar v in
    Hashtbl.add envm x (v,e);
    v

  let mk_vars envm decls = List.map (mk_var envm) decls 

  let mk_in envm decls = 
    let do1 i (x,xs) = 
      let x = 
        match x with 
        | Some s -> s 
        | None -> "input_"^(string_of_int i) in
      mk_var envm (x,None), mk_vars envm xs in
    List.mapi do1 decls 
 
  let of_var envm x = 
    try Hashtbl.find envm x 
    with Not_found -> assert false 

  let ksubst = Parsetree.IK_subst 
  let kglitch = Parsetree.IK_glitch 

  let of_arg envm = function
    | Avar x -> snd (of_var envm x)
    | Aconst c -> P.Eop (mk_op c, [])

  let op_ff s = mk_op s

  let to_expr envm op args = 
    match op, List.map (of_arg envm) args with
    | Oand, [e1; e2] -> ksubst , P.Emul(e1, e2)
    | Oxor, [e1; e2] -> ksubst , P.Eadd(e1, e2)
    | Onot, [e1]     -> ksubst , P.Enot e1
    | Off _s,  es     -> kglitch, List.nth es 1 (* P.Eop(s, es) *)
    | Oid , [e]      -> ksubst , e
    | _   , _        -> assert false 

  let pp_attribute_arg fmt = function
    | AA_int n -> Format.fprintf fmt "%i" n
    | AA_string s -> Format.fprintf fmt "%s" s

  let pp_attribute fmt (id, aa) = 
    Format.fprintf fmt "(* attribute %s %a *) @ " (data id) pp_attribute_arg aa

  let pp_attributes (a,loc) fmt () = 
    Format.fprintf fmt "%a(* from %s*)@ "
      (pp_list "" pp_attribute) a 
      (Location.to_string loc)

  let mk_instr envm i = 
    let i_var = fst (of_var envm i.i_dest) in
    let i_kind, i_expr = to_expr envm i.i_op i.i_args in 
    { P.instr_d = P.Iassgn P.{ i_var; i_kind; i_expr};
      P.instr_info = pp_attributes i.i_attribute }
    
  let func_of_mod modul = 
    let envm = empty_envm () in
    let f_name  = Expr.V.mk_var modul.ilm_name in
    let f_pin   = mk_vars envm modul.ilm_publics in
    let f_in    = mk_in envm modul.ilm_inputs in
    let f_out   = List.map (mk_vars envm) modul.ilm_outputs in
    let f_other = mk_vars envm modul.ilm_others  in
    let f_rand  = mk_vars envm modul.ilm_randoms in
    let f_cmd   = List.map (mk_instr envm) modul.ilm_cmd in
    P.{ f_name; f_pin; f_in; f_out; f_other; f_rand; f_cmd }


end

(* ----------------------------------------------------------- *)
    
let process_file filename =  
  let prog = Parse.read_file filename in
  let mod_ = Process.process_prog prog in
  let func = ToProg.func_of_mod mod_ in
  func  

