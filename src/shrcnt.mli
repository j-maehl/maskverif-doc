(* -------------------------------------------------------------------- *)
type shrcnt

exception Failure

val create     : string -> shrcnt
val id         : shrcnt -> string
val dispose    : shrcnt -> unit
val get        : shrcnt -> int64
val get_update : shrcnt -> int64 -> int64
val update     : shrcnt -> int64 -> unit

val set_unlink_on_dispose : shrcnt -> unit
val clr_unlink_on_dispose : shrcnt -> unit
