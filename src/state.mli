open Expr

(* ----------------------------------------------------------------------- *)
type node

module Hn : Hashtbl.S with type key = node
(* ----------------------------------------------------------------------- *)

module Pinfo : sig

  type t = {
    mutable nb_used_shares : int;
    mutable used_shares    : Util.SmallSet.t;
  }

end

type state

val pp_state : Format.formatter -> state -> unit

val init_state  : int -> param list -> state
val clear_state : state -> unit



(* ----------------------------------------------------------------------- *)

val add_top_expr : state -> expr -> node

(* ----------------------------------------------------------------------- *)

(* [init_todo state] initialize the list of random,
   should be call after new calls to add_top_expr *)
val init_todo  : state -> unit

(* [simplify state] simplify the state by applying random rule
   as much as possible  *)
val simplify : state -> bool

(* [simplified_expr state] return a function
   associating to an expression is new version after
   application of the random rule.
   First call (simplified_expr state) and then call the function
   on expression (whithout modification of the state)
 *)
val simplified_expr : ?notfound:bool -> state -> expr -> expr

(* [simplify_until state k]
   try to simplify the state until the top expression depend of at most
   k share of each input, return true if it success *)
val simplify_until : state -> int -> bool

type bijection
val get_bij : state -> bijection
val replay_bij : state -> bijection -> unit

val clear_bijection : state -> unit

val used_share : state -> (param -> int -> bool)

val nb_used_share : state -> Pinfo.t Hv.t
 
val simplify_until_with_clear : state -> (param -> int -> bool) -> int -> unit

(*
exception CanNotCheck of expr list
val simplify_until_with_clear2 :
  state -> int -> (int * (expr * expr) list * int) list -> unit
 *)
(* ----------------------------------------------------------------------- *)

val is_top_expr : state -> expr -> bool

(* ----------------------------------------------------------------------- *)

val init_class : state -> expr -> unit 
val get_class : state -> expr -> int
