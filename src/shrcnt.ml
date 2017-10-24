(* -------------------------------------------------------------------- *)
type shrcnt

exception Failure

external caml_shrcnt_create   : string -> int -> shrcnt  = "caml_shrcnt_create"
external caml_shrcnt_dispose  : shrcnt -> unit           = "caml_shrcnt_dispose"
external caml_shrcnt_disposed : shrcnt -> bool           = "caml_shrcnt_disposed"
external caml_shrcnt_id       : shrcnt -> string         = "caml_shrcnt_id"
external caml_shrcnt_get      : shrcnt -> int32          = "caml_shrcnt_get"
external caml_shrcnt_update   : shrcnt -> int32 -> int32 = "caml_shrcnt_update"

(* -------------------------------------------------------------------- *)
let () = Callback.register_exception "nu.strub.counter.exn" Failure

(* -------------------------------------------------------------------- *)
let sfc_create : int = 0x0001
let sfc_unlink : int = 0x0002

(* -------------------------------------------------------------------- *)
let create (nm : string) : shrcnt =
  caml_shrcnt_create nm (sfc_create lor sfc_unlink)

(* -------------------------------------------------------------------- *)
let id (the : shrcnt) : string =
  caml_shrcnt_id the

(* -------------------------------------------------------------------- *)
let dispose (the : shrcnt) : unit =
  caml_shrcnt_dispose the

(* -------------------------------------------------------------------- *)
let disposed (the : shrcnt) : bool =
  caml_shrcnt_disposed the

(* -------------------------------------------------------------------- *)
let get (the : shrcnt) : int32 =
  caml_shrcnt_get the

(* -------------------------------------------------------------------- *)
let update (the : shrcnt) (offset : int32) : int32 =
  caml_shrcnt_update the offset
