(* -------------------------------------------------------------------- *)
type shrcnt

exception Failure

val create  : string -> shrcnt
val id      : shrcnt -> string
val dispose : shrcnt -> unit
val get     : shrcnt -> int32
val update  : shrcnt -> int32 -> int32
