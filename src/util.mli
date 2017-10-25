module List : sig
    include module type of List

    val count : ('a -> bool) -> 'a list -> int
    val map_size : ('a -> 'b) -> 'a list -> 'b list * int
    val is_empty : 'a list -> bool
  end

(* ----------------------------------------------------------------------- *)
val partition :
  ('a -> bool) -> 'a list -> 'a list -> 'a list -> 'a list * 'a list

val pp_list :
  ('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  (Format.formatter -> 'e -> unit) -> Format.formatter -> 'e list -> unit

(* ----------------------------------------------------------------------- *)
module Mint : Map.S with type key = int
module Mstr : Map.S with type key = string

(* ----------------------------------------------------------------------- *)
module Array : sig
    include module type of Array

    val for_all : ('a -> bool) -> 'a array -> bool
  end

(* ----------------------------------------------------------------------- *)
exception EmptyStack

module Stack : 
  sig 
    type 'a t
    val make : int  -> 'a -> 'a t
    val push : 'a t -> 'a -> unit
    val pop  : 'a t -> 'a 
    val clear: 'a t -> unit 

    val to_list : 'a t -> 'a list
    val iter : ('a -> unit) -> 'a t -> unit
  end

(* ----------------------------------------------------------------------- *)
val finally : (unit -> 'a) -> ('b -> 'c) -> 'b -> 'c

(* ----------------------------------------------------------------------- *)
module Vector : sig 
  type 'a t 

  val dummy  : unit -> 'a t
  val create : int -> 'a -> 'a t
  val clear  : 'a t -> unit 
  val push   : 'a t -> 'a -> unit
  val size   : 'a t -> int
  val pop    : 'a t -> 'a 
  val top    : 'a t -> 'a

  (* remove all elements satisfying the predicate *)
  val remove : ('a -> bool) -> 'a t -> unit

  val iter   : ('a -> unit) -> 'a t -> unit
  val exists : ('a -> bool) -> 'a t -> bool

  val to_list : 'a t -> 'a list                                       
end
