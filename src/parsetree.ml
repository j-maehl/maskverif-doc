open Util

 


(* -------------------------------------------------------------------- *)
exception ParseError of Location.t * string option

(* -------------------------------------------------------------------- *)
type ident = string located

module OrderedId = struct
  type t = ident
  let compare x y = compare (data x) (data y)
end

module Mid = Map.Make(OrderedId)
module Sid = Set.Make(OrderedId)

type expr_r = 
  | Evar of ident
  | Eadd of expr * expr 
  | Emul of expr * expr
  | Enot of expr
(*  | Eglitch of expr
    | Ehide   of expr *)

and expr = expr_r located

type instr_kind = 
  | IK_subst
  | IK_hide
  | IK_sub
  | IK_glitch

type assgn = {i_var : ident; i_kind : instr_kind; i_expr : expr }

type vcall = 
  | Vid of ident 
  | Vtuple of (ident list) located

type vcalls = vcall list

type macro_call = { i_lhs : vcalls; i_macro: ident; i_args : vcalls }

type instr =
  | Iassgn of assgn
  | Imacro of macro_call 

type cmd = instr list

type func = {
  f_name   : ident;
  f_in     : (ident * ident list) list;
  f_out    : (ident * ident list) list;
  f_shares : (ident * ident list) list;
  f_rand   : ident list;
  f_cmd    : cmd }

type prog = func list

(* --------------------------------------------------------- *)

let pp_ident fmt x = 
  Format.fprintf fmt "%s" (data x)

let pp_decl fmt (x,xs) = 
  Format.fprintf fmt "@[<hov> %a = %a@]"
    pp_ident x (pp_list " +@ " pp_ident) xs

let pp_decls fmt ds = 
  Format.fprintf fmt "@[<hov>%a@]" (pp_list ",@ " pp_decl) ds

type side = Left | Right

type level = 
  | Top
  | Add
  | Mul
  | Not

let pp_box level_up level = 
  let c = match level_up, level with
    | (Top,_), _ -> false
    | (Add, _), (Top,_) -> true
    | (Add, Right), (Add, _) -> true
    | (Add,_), _ -> false
    | (Mul,_), ((Top,_) | (Add, _)) -> true
    | (Mul, Right), (Mul, _) -> true
    | (Mul,_), _ -> false
    | (Not,_), ((Top,_) | (Add, _) | (Mul, _)) -> true
    | (Not,_), _ -> false
    in
  pp_maybe_paren c

let rec pp_expr_l fmt (level, e) = 
  match data e with
  | Evar x -> pp_ident fmt x
  | Eadd(e1, e2) ->
    let pp fmt (e1, e2) = 
      Format.fprintf fmt "@[<hov>%a +@ %a@]"
        pp_expr_l ((Add,Left), e1) pp_expr_l ((Add,Right), e2) in
    pp_box level (Add,Left) pp fmt (e1,e2)
  | Emul(e1, e2) -> 
    let pp fmt (e1, e2) = 
      Format.fprintf fmt "@[<hov>%a *@ %a@]"
        pp_expr_l ((Mul,Left), e1) pp_expr_l ((Mul,Right), e2) in
    pp_box level (Mul,Left) pp fmt (e1,e2)
  | Enot e ->
    Format.fprintf fmt "@[<hov>!%a@]" pp_expr_l ((Not,Left), e)

and pp_expr fmt e = pp_expr_l fmt ((Top,Left), e)
  
let pp_assgn fmt i =
  begin match i.i_kind with
  | IK_subst ->
    Format.fprintf fmt "@[<hov 2> %a :=@ %a@]" 
  | IK_hide ->
    Format.fprintf fmt "@[<hov 2> %a =@ {%a}@]" 
  | IK_sub ->
    Format.fprintf fmt "@[<hov 2> %a =@ %a@]" 
  | IK_glitch ->
    Format.fprintf fmt "@[<hov 2> %a =@ [%a]@]" 
  end
     pp_ident i.i_var pp_expr i.i_expr

let pp_vcall fmt = function
  | Vid x -> pp_ident fmt x
  | Vtuple xs -> 
    Format.fprintf fmt "[@[<hov 2>%a@]]" (pp_list ",@ " pp_ident) (data xs)

let pp_vcalls fmt xs = 
  Format.fprintf fmt "(@[<hov 2>%a@])" (pp_list ",@ " pp_vcall) xs
  
let pp_call fmt i = 
   Format.fprintf fmt "@[<hov 2> %a =@ %a%a@]" 
     pp_vcalls i.i_lhs pp_ident i.i_macro pp_vcalls i.i_args 

let pp_instr fmt = function 
  | Iassgn i -> pp_assgn fmt i
  | Imacro  i -> pp_call fmt i
 
let pp_cmd fmt c = 
  Format.fprintf fmt "@[<v>%a@]" (pp_list "@ " pp_instr) c

let pp_func fmt func = 
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "@[<hov>inputs : %a@]@ " pp_decls func.f_in;
  Format.fprintf fmt "@[<hov>outputs: %a@]@ " pp_decls func.f_out;
  Format.fprintf fmt "@[<hov>shares : %a@]@ " pp_decls func.f_shares;
  Format.fprintf fmt "@[<hov>randoms: %a;@]@ @ "
    (pp_list ",@ " pp_ident) func.f_rand;
  pp_cmd fmt func.f_cmd;
  Format.fprintf fmt "@]"

let pp_prog fmt prog = 
  Format.fprintf fmt "@[<v>%a@]"
    (pp_list "@ @ " pp_func) prog 


(* --------------------------------------------------------------------- *)
let vcall_loc = function 
  | Vid id -> loc id
  | Vtuple ids -> loc ids 
