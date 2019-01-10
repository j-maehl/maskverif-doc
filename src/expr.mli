type var = private {
  v_id   : int;
  v_name : string;
}              

module V : sig
  val mk_var  : string -> var
  val clone   : var -> var
  val equal   : var -> var -> bool
  val compare : var -> var -> int
end 

module Sv : Set.S with type elt = var
module Mv : Map.S with type key = var
module Hv : Hashtbl.S with type key = var

type rnd = var 

type param = var 

type operator = Util.hstring 

type expr = private {
  e_id : int;
  e_node : expr_node
}

and expr_node = 
| Etop
| Ernd   of rnd
| Eshare of param * int * var
| Epub   of var 
| Eneg   of expr
| Eadd   of expr * expr
| Emul   of expr * expr
| Eop    of bool * operator * expr array
  (* Invariant [Etuple(b,es)]
       if b is true there is no duplicate in the array es *)

module E : sig
  val mk_expr : expr_node -> expr 
  val equal : expr -> expr -> bool
  val hash  : expr -> int 
  val compare : expr -> expr -> int 
end

module He : Hashtbl.S with type key = expr

module Se : Set.S with type elt = expr

module Me : Map.S with type key = expr 

val top   : expr
val rnd   : rnd -> expr 
val share : param -> int -> var -> expr 
val pub   : var -> expr 
val neg   : expr -> expr
val add   : expr -> expr -> expr 
val mul   : expr -> expr -> expr 
val op    : operator -> expr array -> expr
val tuple : expr array -> expr
val efalse : expr
val etrue  : expr

val is_op_tuple : operator -> bool
val is_rnd      : expr -> bool


(* [unsafe_op b es]: [b] should be true only if [es] has no duplicate *)
val unsafe_op : bool -> operator -> expr array -> expr

(* [tuple_nodup es]: [es] should not contains duplicate *)
val tuple_nodup : expr array -> expr
val tuple : expr array -> expr


val pp_var    : Format.formatter -> var -> unit
val pp_rnd    : Format.formatter -> rnd -> unit
val pp_share  : Format.formatter -> param * int * var -> unit

val pp_expr : Format.formatter -> expr -> unit

(* --------------------------------------------------------------- *)
type result = 
  | Rb of bool
  | Rtuple of result array 

exception CheckBool
val check_bool : Util.tool_opt -> expr -> unit
