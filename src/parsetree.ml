open Util

(* -------------------------------------------------------------------- *)
type ident = string located

module OrderedId = struct
  type t = ident
  let compare x y = compare (data x) (data y)
end

module Mid = Map.Make(OrderedId)
module Sid = Set.Make(OrderedId)

type range = int * int

type rangen = (int * int) list

type shift =
  | Sr of int
  | Sl of int

type id_range = ident * (rangen option)

type vcall1 =
  | Vid of id_range
  | Vtuple of (ident list) located

type vcall = vcall1 * shift option

type expr_r =
  | Evar of vcall
  | Econst of Z.t located * Expr.ty 
  | Eop  of ident * expr list

and expr = expr_r located

type instr_kind =
  | IK_subst
  | IK_hide
  | IK_sub
  | IK_glitch
  | IK_noleak

type assgn = {i_var : vcall; i_kind : instr_kind; i_expr : expr }

type vcalls = vcall list

type macro_call = { i_lhs : vcalls; i_macro: ident; i_args : vcalls }

type leak = { l_name: ident; l_exprs : expr list }

type instr =
  | Ileak of leak * string
  | Iassgn of assgn
  | Imacro of macro_call

type cmd = (instr located) list

type ids =
  | Ids   of ident list
  | Range of range

type func = {
  f_name   : ident;
  f_pin    : (ident * range option * Expr.ty) list;
  f_in     : (ident * ids * Expr.ty) list;
  f_out    : (ident * ids * Expr.ty) list;
  f_shares : (ident * ids * Expr.ty) list;
  f_rand   : (ident * range option * Expr.ty) list;
  f_other  : (ident * Expr.ty) list;
  f_kind   : proc_kind;
  f_cmd    : cmd;
  f_pout   : vcall1 list;
 }

type checker_option =
  | Order of int
  | NoGlitch
  | Para
  | NoBool
  | NoPrint
  | Transition

type checker_options = checker_option list

type ty_op = (bool * Expr.ty)list * Expr.ty 

type command =
  | Func       of func
  | Operator   of ident * ty_op * bool
  | NI         of ident * checker_options
  | SNI        of ident * (int * int) option * checker_options
  | SPINI      of ident * (int * int) option * checker_options
  | Probing    of ident * checker_options
  | Read_file  of string located
  | Read_ilang of string located
  | Print      of ident
  | Reset
  | Verbose    of int located
  | Exit

(* --------------------------------------------------------- *)

let pp_ident fmt x =
  Format.fprintf fmt "%s" (data x)

let pp_range fmt (i,j) =
  Format.fprintf fmt "[%i:%i]" i j

let pp_ids fmt = function
  | Ids xs -> pp_list " +@ " pp_ident fmt xs
  | Range r -> pp_range fmt r

let pp_decl fmt (x,xs) =
  Format.fprintf fmt "@[<hov> %a = %a@]" pp_ident x pp_ids xs

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

let pp_range1 fmt (i,j) =
  if i=j then Format.fprintf fmt "%i" i
  else Format.fprintf fmt "%i:%i" i j

let pp_rangen fmt r =
  Format.fprintf fmt "[@[<hov>%a@]]"
    (pp_list ",@ " pp_range1) r

let pp_option pp fmt o =
  match o with
  | None -> ()
  | Some x -> pp fmt x

let pp_shift fmt = function
  | Sr i -> Format.fprintf fmt ">>%i" i
  | Sl i -> Format.fprintf fmt "<<%i" i

let pp_id_range fmt (x, range) =
  Format.fprintf fmt "%a%a"
    pp_ident x (pp_option pp_rangen) range

let pp_vcall1 fmt = function
  | Vid x -> pp_id_range fmt x
  | Vtuple xs ->
    Format.fprintf fmt "[@[<hov 2>%a@]]" (pp_list ",@ " pp_ident) (data xs)

let pp_vcall fmt (x,s) =
  Format.fprintf fmt "%a%a" pp_vcall1 x (pp_option pp_shift) s
(*
let rec pp_expr_l fmt (level, e) =
  match data e with
  | Evar x -> pp_vcall fmt x
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
  | IK_noleak ->
    Format.fprintf fmt "@[<hov 2> %a <-@ {%a}@]"
  end
     pp_vcall i.i_var pp_expr i.i_expr

let pp_vcalls fmt xs =
  Format.fprintf fmt "(@[<hov 2>%a@])" (pp_list ",@ " pp_vcall) xs

let pp_call fmt i =
   Format.fprintf fmt "@[<hov 2> %a =@ %a%a@]"
     pp_vcalls i.i_lhs pp_ident i.i_macro pp_vcalls i.i_args

let pp_exprs fmt es =
  Format.fprintf fmt "(@[<hov 2>%a@])" (pp_list ",@ " pp_expr) es

let pp_leak fmt i msg =
  let pp_msg fmt msg = 
    if msg = "" then ()
    else Format.fprintf fmt "  \"%s\"" msg in
  Format.fprintf fmt "@[<hov 2> %a |=@ (%a)%a@]"
    pp_ident i.l_name pp_exprs i.l_exprs pp_msg msg 

let pp_instr fmt i =
  match data i with
  | Iassgn i -> pp_assgn fmt i
  | Imacro  i -> pp_call fmt i
  | Ileak (i,msg) -> pp_leak fmt i msg

let pp_cmd fmt c =
  Format.fprintf fmt "@[<v>%a@]" (pp_list "@ " pp_instr) c

let pp_id_range_decl fmt (id,r) =
  Format.fprintf fmt "%a%a" pp_ident id (pp_option pp_range1) r

let pp_func fmt func =
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "@[<hov>public inputs: %a;@]@ @ "
    (pp_list ",@ " pp_id_range_decl) func.f_pin;
  Format.fprintf fmt "@[<hov>inputs : %a@]@ " pp_decls func.f_in;
  Format.fprintf fmt "@[<hov>outputs: %a@]@ " pp_decls func.f_out;
  Format.fprintf fmt "@[<hov>shares : %a@]@ " pp_decls func.f_shares;
  Format.fprintf fmt "@[<hov>randoms: %a;@]@ @ "
    (pp_list ",@ " pp_id_range_decl) func.f_rand;
  pp_cmd fmt func.f_cmd;
  Format.fprintf fmt "@]"

let pp_prog fmt prog =
  Format.fprintf fmt "@[<v>%a@]"
    (pp_list "@ @ " pp_func) prog
 *)

(* --------------------------------------------------------------------- *)
let vcall1_loc = function
  | Vid (id,_) -> loc id
  | Vtuple ids -> loc ids

let vcall_loc v = vcall1_loc (fst v)

