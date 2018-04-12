open Util

module P = Parsetree
module E = Expr

type var = E.var 
  
type expr = 
  | Evar of var
  | Eadd of expr * expr 
  | Emul of expr * expr
  | Enot of expr

type assgn = {i_var : var; i_kind : P.instr_kind; i_expr : expr }

type vcall = var list 

type vcalls = vcall list

type fname = E.var

type macro_call = { i_lhs : vcalls; i_macro: fname; i_args : vcalls }

type instr =
  | Iassgn of assgn
  | Imacro of macro_call 

type cmd = instr list

type func = {
  f_name   : fname;
  f_in     : (var * var list) list;
  f_out    : var list list;
  f_other  : var list;
  f_rand   : var list;
  f_cmd    : cmd }

type prog = func list

(* ------------------------------------------------------------------- *)
(* Pretty printing                                                     *)

let pp_var full fmt x = 
  let pp_id fmt i = 
    if full then Format.fprintf fmt "#%i" i in
  Format.fprintf fmt "%s%a" x.E.v_name pp_id x.E.v_id


let pp_expr full fmt e = 
  let rec aux top fmt e = 
    match e with
    | Evar x      -> pp_var full fmt x
    | Enot e      -> Format.fprintf fmt "!%a" (aux `Not) e
    | Eadd(e1,e2) -> 
      begin match top with
      | `AddR | `MulL | `MulR | `Not ->
        Format.fprintf fmt "(%a + %a)" (aux `AddL) e1 (aux `AddR) e2
      | _ -> 
        Format.fprintf fmt "%a + %a" (aux `AddL) e1 (aux `AddR) e2
      end
    | Emul(e1,e2) -> 
      begin match top with
      | `MulR | `Not -> 
        Format.fprintf fmt "(%a.%a)" (aux `MulL) e1 (aux `MulR) e2
      | _ ->
        Format.fprintf fmt "%a.%a" (aux `MulL) e1 (aux `MulR) e2
      end in
  aux `Top fmt e

let pp_assgn full fmt i =
  begin match i.i_kind with
  | P.IK_subst ->
    Format.fprintf fmt "@[<hov 2> %a :=@ %a@]" 
  | P.IK_hide ->
    Format.fprintf fmt "@[<hov 2> %a =@ {%a}@]" 
  | P.IK_sub ->
    Format.fprintf fmt "@[<hov 2> %a =@ %a@]" 
  | P.IK_glitch ->
    Format.fprintf fmt "@[<hov 2> %a =@ [%a]@]" 
  end
     (pp_var full) i.i_var (pp_expr full) i.i_expr

let pp_vars full fmt xs = 
  Format.fprintf fmt "@[<hov>%a@]" (pp_list ",@ " (pp_var full)) xs

let pp_vcall full fmt xs = 
  Format.fprintf fmt "[%a]" (pp_vars full) xs

let pp_vcalls full fmt xs = 
  Format.fprintf fmt "@[<hov>(%a)@]"
    (pp_list ",@ " (pp_vcall full)) xs

let pp_call full fmt i = 
  Format.fprintf fmt "@[<hov 2> %a =@ %a%a@]" 
    (pp_vcalls full) i.i_lhs (pp_var full) i.i_macro (pp_vcalls full) i.i_args 
  
let pp_instr full fmt = function 
  | Iassgn i -> pp_assgn full fmt i
  | Imacro  i -> pp_call full fmt i
 
let pp_cmd full fmt c = 
  Format.fprintf fmt "@[<v>%a@]" (pp_list "@ " (pp_instr full)) c

let pp_decls full fmt xs = 
  Format.fprintf fmt "@[<hov>%a@]" (pp_list ",@ " (pp_vcall full)) xs

let pp_indecls full fmt xs = 
  let pp fmt (x,xs) = 
    Format.fprintf fmt "(%a, %a)" (pp_var full) x (pp_vcall full) xs in
  Format.fprintf fmt "@[<hov>%a@]" (pp_list ",@ " pp) xs

let pp_func full fmt func = 
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "@[<hov>inputs : %a@]@ " (pp_indecls full) func.f_in;
  Format.fprintf fmt "@[<hov>outputs: %a@]@ " (pp_decls full) func.f_out;
  Format.fprintf fmt "@[<hov>randoms: %a@]@ "
    (pp_list ",@ " (pp_var full)) func.f_rand;
  Format.fprintf fmt "@[<hov>others : %a;@]@ @ "
    (pp_list ",@ " (pp_var full)) func.f_other;
  pp_cmd full fmt func.f_cmd;
  Format.fprintf fmt "@]"

let pp_prog full fmt prog = 
  Format.fprintf fmt "@[<v>%a@]"
    (pp_list "@ @ " (pp_func full)) prog 

(* ------------------------------------------------------------------------ *)
(* Translation from Parsetree to Prog *)

type vkind = 
  | VKvar of var 
  | VKtuple  of var list

type env = {
            macro  : func P.Mid.t;
    mutable locals : vkind P.Mid.t;
    mutable others : var list;
    mutable init   : E.Sv.t
  }

let error loc = error "Type error" (Some loc) 

let check_init env id k = 
  match k with
  | VKvar x -> 
    if not (E.Sv.mem x env.init) then 
      error (loc id) "%a is not initialized" P.pp_ident id
  | VKtuple xs ->
    let l = List.filter (fun x -> not (E.Sv.mem x env.init)) xs in
    if l <> [] then
      error (loc id) "some fields of %a are not initialized %a"
         P.pp_ident id (pp_list ",@ " (pp_var false)) xs

let get_var_kind env id =
  let k = 
    try P.Mid.find id env.locals
    with Not_found -> error (loc id) "%a is unknown" P.pp_ident id in
  check_init env id k; k

let check_kind_var id k = 
  match k with
  | VKvar v -> v
  | VKtuple _ ->  error (loc id) "%a is a shared variable" P.pp_ident id

let check_kind_tuple id k = 
  match k with
  | VKvar _ -> error (loc id) "%a is not a shared variable" P.pp_ident id
  | VKtuple vs -> vs

let get_var_expr env id =
  let k = get_var_kind env id in
  check_kind_var id k 
 
let rec to_expr env e = 
  match data e with 
  | P.Evar id      -> Evar (get_var_expr env id)
  | P.Eadd(e1, e2) -> Eadd (to_expr env e1, to_expr env e2)
  | P.Emul(e1, e2) -> Emul (to_expr env e1, to_expr env e2)
  | P.Enot e       -> Enot (to_expr env e)

let add_var other env id kind = 
  if P.Mid.mem id env.locals then
    error (loc id) "multiple declaration of %a" P.pp_ident id;
  env.locals <- P.Mid.add id kind env.locals;
  match other, kind with
  | true, VKvar x -> env.others <- x::env.others
  | _ -> ()

let set_init env x = 
  env.init <- E.Sv.add x env.init 

let set_rand env id = 
  let x = E.V.mk_var (data id) in
  add_var false env id (VKvar x);
  set_init env x;
  x

let set_shared other env (id,ids) = 
  let add id = 
    let x = E.V.mk_var (data id) in
    add_var other env id (VKvar x);
    x in
  let xs = List.map add ids in
  let x = E.V.mk_var (data id) in
  add_var other env id (VKtuple xs);
  x, xs

let set_shared_init other env ids = 
  let (_, xs as xxs) = set_shared other env ids in
  List.iter (set_init env) xs;
  xxs

let set_var_assgn env id = 
  let k = 
    try P.Mid.find id env.locals 
    with Not_found -> 
      let x = VKvar (E.V.mk_var (data id)) in
      add_var true env id x;
      x in
  let x = check_kind_var id k in
  set_init env x;
  x
 
let to_assgn env i = 
  { i_var  = set_var_assgn env i.P.i_var;
    i_kind = i.P.i_kind;
    i_expr = to_expr env i.P.i_expr }

let set_var_call env vcall out = 
  let xs = 
    match vcall with
    | P.Vid id ->
      let k = 
        try P.Mid.find id env.locals
        with Not_found -> 
          error (loc id) "undeclared shared variable %a" P.pp_ident id in
      let xs = check_kind_tuple id k in
      List.iter (set_init env) xs;
      xs

   | P.Vtuple ids ->
     List.map (set_var_assgn env) (data ids) in
  let l1 = List.length xs in
  let l2 =  List.length out in
  if l1 <> l2 then
    error (P.vcall_loc vcall) 
      "the argument %a has %i shares while %i is expected"
      P.pp_vcall vcall l1 l2;
    xs 

let set_var_calls env id vcalls outs = 
  let l1 = List.length outs in
  let l2 = List.length vcalls in
  if l1 <> l2 then
    error (loc id) 
      "the function %a return %i results while %i is expected" 
      P.pp_ident id l1 l2;
  List.map2 (set_var_call env) vcalls outs

let get_var_call env vcall in_ = 
  let xs = 
    match vcall with
    | P.Vid id ->
      let k = 
        try P.Mid.find id env.locals
        with Not_found -> 
          error (loc id) "undeclared shared variable %a" P.pp_ident id in
      let xs = check_kind_tuple id k in
      check_init env id k;
      xs 
    | P.Vtuple ids ->
      List.map (get_var_expr env) (data ids) in
  let l1 = List.length xs in
  let l2 =  List.length (snd in_) in
  if l1 <> l2 then
    error (P.vcall_loc vcall) 
      "the argument %a has %i shares while %i is expected"
      P.pp_vcall vcall l1 l2;
    xs 

let get_var_calls env id vcalls ins = 
  let l1 = List.length ins in
  let l2 = List.length vcalls in
  if l1 <> l2 then
    error (loc id) 
      "the function %a expects %i arguments while %i are provided" 
      P.pp_ident id l1 l2;
  List.map2 (get_var_call env) vcalls ins
  
let to_macro env i = 
  let id = i.P.i_macro in
  let f = 
    try P.Mid.find id env.macro
    with Not_found -> 
      error (loc id) "undeclared macro %a" P.pp_ident id in 
  let i_macro = f.f_name in
  let i_args = get_var_calls env id i.P.i_args f.f_in in
  let i_lhs = set_var_calls env id i.P.i_lhs f.f_out in
  { i_lhs; i_macro; i_args }

let to_instr env i = 
  match i with 
  | P.Iassgn i -> Iassgn (to_assgn env i)
  | P.Imacro i -> Imacro (to_macro env i)


let init_shared macros func = 
  let env = { macro = macros; 
              locals = P.Mid.empty; 
              others = []; 
              init = E.Sv.empty } in
  let f_in = List.map (set_shared_init false env) func.P.f_in in
  let f_ou = List.map (set_shared false env) func.P.f_out in
  let f_ou = List.map snd f_ou in
  let _    = List.map (set_shared true env) func.P.f_shares in
  let rnds = List.map (set_rand env) func.P.f_rand in
  env, f_in, f_ou, rnds 

let to_func macros func = 
  let f_name = E.V.mk_var (data func.P.f_name) in
  let env, f_in, f_out, f_rand = init_shared macros func in
  let f_cmd = List.map (to_instr env) func.P.f_cmd in
  let f_other = env.others in
  { f_name; f_in; f_out; f_other; f_rand; f_cmd }

let to_prog p = 
  let macros = ref P.Mid.empty in
  let add_func func = 
    let f = to_func !macros func in
    macros := P.Mid.add func.P.f_name f !macros;
    f in
  List.map add_func p




(* ------------------------------------------------------------------ *)
let rename_var s x = 
  try E.Hv.find s x with Not_found -> assert false 

let rec rename_e s e =
  match e with
  | Evar x -> Evar (rename_var s x)
  | Eadd(e1, e2) -> Eadd(rename_e s e1, rename_e s e2)
  | Emul(e1, e2) -> Emul(rename_e s e1, rename_e s e2)
  | Enot e       -> Enot(rename_e s e)

let rename_i s i = 
  match i with
  | Iassgn i ->
    Iassgn { i with i_var = rename_var s i.i_var;
             i_expr = rename_e s i.i_expr }
  | Imacro i ->
    let rename_vcalls = List.map (List.map (rename_var s)) in
    Imacro { i_lhs   = rename_vcalls i.i_lhs;
             i_macro = i.i_macro;
             i_args  = rename_vcalls i.i_args }

(* ------------------------------------------------------------------ *)

type env_expand = {
   macro : func E.Hv.t; 
   mutable rnd : var list;  
   mutable other : var list;
  }

let get_macro env fname = 
  try E.Hv.find env.macro fname 
  with Not_found -> assert false 

let macro_expand_call env i = 
  let func = get_macro env i.i_macro in
  let s = E.Hv.create 57 in
  let add x y = E.Hv.add s x y in
  let adds xs ys = List.iter2 add xs ys in
  List.iter2 adds (List.map snd func.f_in) i.i_args;
  List.iter2 adds func.f_out i.i_lhs;
  let add_clone x = let x' = E.V.clone x in add x x'; x' in
  let add_rnd x   = let x' = add_clone x in env.rnd <- x'::env.rnd in
  let add_other x = let x' = add_clone x in env.other <- x'::env.other in
  List.iter add_rnd func.f_rand;
  List.iter add_other func.f_other;
  List.map (rename_i s) func.f_cmd
  
let rec macro_expand_c env c =
  match c with
  | [] -> []
  | Iassgn _ as i :: c -> i :: macro_expand_c env c 
  | Imacro i :: c ->
    let c = macro_expand_c env c in
    let ic = macro_expand_call env i in
    ic@c

let macro_expand_func macro func = 
  let env = {
      macro = macro;
      other = func.f_other;
      rnd   = func.f_rand;
    } in
  let c = macro_expand_c env func.f_cmd in
  let func = 
    { func with 
      f_other = env.other;
      f_rand  = env.rnd;
      f_cmd   = c } in
  E.Hv.add macro func.f_name func;
  func

let macro_expand_prog prog = 
  let macro = E.Hv.create 10 in
  List.map (macro_expand_func macro) prog

     
(* ----------------------------------------------------------- *)

let simplify_func f =
  let s = E.Hv.create 17 in
  let add_var x e = E.Hv.replace s x e in
  let add_vdecl x = add_var x (Evar x) in
  let add_vdecls xs = List.iter add_vdecl xs in
  (* Inititalisation *)
  List.iter add_vdecls (List.map snd f.f_in);
  add_vdecls f.f_rand;

  (* Substitution *)
  let subst_v v = try E.Hv.find s v with Not_found -> assert false in
  let rec subst e = 
    match e with
    | Evar x      -> subst_v x
    | Eadd(e1,e2) -> Eadd(subst e1, subst e2)
    | Emul(e1,e2) -> Emul(subst e1, subst e2)
    | Enot e      -> Enot(subst e)  in

  (* Simplification of the code *)
  let rec simplify_cmd c = 
    match c with
    | [] -> []
    | Iassgn i :: c ->
      let e = subst i.i_expr in
      if i.i_kind = P.IK_subst then 
        (add_var i.i_var e;
         simplify_cmd c)
      else
        let i' = Iassgn { i with i_expr = e } in
        add_vdecl i.i_var;
        let c = simplify_cmd c in
        i'::c
    | Imacro _ :: _ -> assert false in
  
  { f with f_cmd = simplify_cmd f.f_cmd }

let simplify_prog = List.map simplify_func  

 
(* ------------------------------------------------------------------ *)
(* Buildind the set of possible observations *)
open Expr 

let expr_of_var s x = 
  try Hv.find s x 
  with Not_found -> assert false

let rec expr_of_pexpr s e = 
  match e with
  | Evar x      -> expr_of_var s x
  | Eadd(e1,e2) -> add (expr_of_pexpr s e1) (expr_of_pexpr s e2)
  | Emul(e1,e2) -> mul (expr_of_pexpr s e1) (expr_of_pexpr s e2)
  | Enot e      -> neg (expr_of_pexpr s e)

let fv = 
  let rec aux fv e = 
    match e with
    | Evar x -> Sv.add x fv 
    | Eadd(e1,e2) | Emul(e1,e2) -> aux (aux fv e1) e2
    | Enot e -> aux fv e in
  aux Sv.empty

let rec build_obs obs s c = 
  match c with 
  | [] -> obs
  | Imacro _ :: _ -> assert false 
  | Iassgn i :: c ->
    let e = expr_of_pexpr s i.i_expr in
    let obs = 
      match i.i_kind with
      | P.IK_sub ->
        fst (add_sub obs s i.i_expr)
      | P.IK_glitch ->
        let fv = Sv.elements (fv i.i_expr) in
        let es = Se.of_list (List.map (expr_of_var s) fv) in
        Se.add (tuple_nodup (Array.of_list (Se.elements es))) obs
      | P.IK_hide -> Se.add e obs 
      | P.IK_subst -> assert false in
    Hv.replace s i.i_var e;
    build_obs obs s c 
      
and add_sub obs s e = 
  match e with
  | Evar x      -> 
    let e = expr_of_var s x in
    Se.add e obs, e
  | Eadd(e1,e2) -> 
    let obs, e1 = add_sub obs s e1 in
    let obs, e2 = add_sub obs s e2 in
    let e = add e1 e2 in
    Se.add e obs, e 
  | Emul(e1,e2) -> 
    let obs, e1 = add_sub obs s e1 in
    let obs, e2 = add_sub obs s e2 in
    let e = mul e1 e2 in
    Se.add e obs, e
  | Enot e      -> 
    let obs, e = add_sub obs s e in
    obs, neg e 
    (* We do not the (not e) but simply e since e and (not e) 
       equivalent for the adversary *)

let build_obs_func f =
  let nb_shares = ref 0 in
  let check_shares xs = 
    let xs = Array.of_list xs in
    if !nb_shares = 0 then nb_shares := Array.length xs;
    if !nb_shares <> Array.length xs then assert false;
    xs in
  let s = Hv.create 101 in
  let add_subst x p = Hv.add s x p in
  let add_params (x, xs) = 
    let xs = check_shares xs in
    let mk_param i xi = 
      let p = share x i in
      add_subst xi p in
    Array.iteri mk_param xs;
    x
  in
  (* Build the parameters and the substitution *) 
  let params = List.map add_params f.f_in in
  (* Build the randoms *)
  let obs = ref Se.empty in
  let add_random x = 
    let r = rnd x in
    obs := Se.add r !obs;
    add_subst x r in
  List.iter add_random f.f_rand; 
  let obs = build_obs !obs s f.f_cmd in
  (* Compute the set of output *)
  let out = 
    List.fold_left 
      (List.fold_left (fun obs x -> Se.add (expr_of_var s x) obs))
      Se.empty f.f_out in
  (* Remove out from the set of obs *)
  let interns = Se.diff obs out in
  (params, !nb_shares, Se.elements interns, Se.elements out)
 
