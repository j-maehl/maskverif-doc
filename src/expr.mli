type var = private {
  v_id   : int;
  v_name : string;
}              

module V : sig
  val mk_var  : string -> var
  val equal   : var -> var -> bool
  val compare : var -> var -> int
end 

module Hv : Hashtbl.S with type key = var

type rnd = var 

type param = var 

type expr = private {
  e_id : int;
  e_node : expr_node
}

and expr_node = 
| Etop
| Ernd   of rnd
| Eshare of param * int
| Eadd   of expr * expr
| Emul   of expr * expr


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
val share : param -> int -> expr 
val add   : expr -> expr -> expr 
val mul   : expr -> expr -> expr 

val pp_var    : Format.formatter -> var -> unit
val pp_rnd    : Format.formatter -> rnd -> unit
val pp_share  : Format.formatter -> param * int -> unit

val pp_expr : Format.formatter -> expr -> unit

