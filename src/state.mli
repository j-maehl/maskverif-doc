open Util
open Expr

(* ----------------------------------------------------------------------- *)
type node

module Hn : Hashtbl.S with type key = node
(* ----------------------------------------------------------------------- *)

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

module Pinfo : sig 

  type t = {
    mutable nb_used_shares : int; 
    mutable used_shares    : SmallSet.t;
  }

  val empty : unit -> t

  val add_share : int -> t -> unit 

  val remove_share : int -> t -> unit 

  val clear : t -> unit
end

(* [simplify_until state k] 
   try to simplify the state until the top expression depend of at most 
   k share of each input, return true if it success *)
type t_continue = Pinfo.t Hv.t -> bool

val simplify_until : t_continue -> state -> bool 
val continue_k : int -> t_continue
val continue_spini : int -> t_continue
type bijection
val get_bij : state -> bijection
val replay_bij : state -> bijection -> unit

val clear_bijection : state -> unit

val used_share : state -> (param -> int -> bool)

val simplify_until_with_clear : t_continue -> state -> (param -> int -> bool) -> unit 

(* ----------------------------------------------------------------------- *)

val is_top_expr : state -> expr -> bool

(* ----------------------------------------------------------------------- *)
val init_class : state -> expr -> unit
val get_class : state -> expr -> int

