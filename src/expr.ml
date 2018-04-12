open Util
type var = {
  v_id   : int;
  v_name : string;
}

module V = struct 

  let id = ref 0 

 (* let tbl = Hashtbl.create 100 

  let mk_var s =
    try Hashtbl.find tbl s 
    with Not_found ->
      let p = { v_id = !id; v_name = s } in
      incr id;
      Hashtbl.add tbl s p;
      p *)

  let mk_var s =
    let p = { v_id = !id; v_name = s } in
    incr id;
    p 
    
  let clone v = mk_var v.v_name

  type t = var 

  let equal v1 v2 = v1 == v2

  let hash v = v.v_id

  let compare v1 v2 = v1.v_id - v2.v_id
    
end

module Sv = Set.Make(V)
module Mv = Map.Make(V)
module Hv = Hashtbl.Make(V)

(* ----------------------------------------------------------------------- *)

type rnd = var
type param = var 

let pp_var fmt v = Format.fprintf fmt "%s" v.v_name

let pp_rnd fmt r = Format.fprintf fmt "$%a" pp_var r

let pp_share fmt (p,s) = Format.fprintf fmt "%a%i" pp_var p s

(* ----------------------------------------------------------------------- *)

type expr = {
  e_id : int;
  e_node : expr_node}

and expr_node = 
| Etop
| Ernd   of rnd
| Eshare of param * int
| Eneg   of expr
| Eadd   of expr * expr
| Emul   of expr * expr
| Etuple of bool * expr array
  (* Invariant [Etuple(b,es)]
       if b is true there is no duplicate in the array es *)

let e_equal e1 e2 = e1 == e2
let e_hash e = e.e_id

module HE = struct

  type t = expr

  let equal_node n1 n2 = 
    match n1, n2 with 
    | Etop         , Etop          -> true
    | Ernd r1      , Ernd r2       -> V.equal r1 r2
    | Eshare(p1,i1), Eshare(p2,i2) -> i1 == i2 && V.equal p1 p2
    | Eneg e1      , Eneg e2       -> e_equal e1 e2
    | Eadd(e11,e12), Eadd(e21,e22) -> e_equal e11 e21 && e_equal e12 e22
    | Emul(e11,e12), Emul(e21,e22) -> e_equal e11 e21 && e_equal e12 e22
    | Etuple(b1,es1), Etuple(b2,es2) -> 
      b1 = b2 && Array.for_all2 e_equal es1 es2
    | _                            -> false

  let equal e1 e2 = equal_node e1.e_node e2.e_node 

  let combine h tag = h lsl 3 + tag
  let combine2 h1 h2 tag = combine (combine h1 h2) tag

  let hash_node = function
    | Etop        -> 0
    | Ernd r      -> combine (V.hash r) 1
    | Eshare(p,i) -> combine2 (V.hash p) i 2
    | Eneg e1     -> combine (e_hash e1) 3 
    | Eadd(e1,e2) -> combine2 (e_hash e1) (e_hash e2) 4
    | Emul(e1,e2) -> combine2 (e_hash e1) (e_hash e2) 5 
    | Etuple(_b,es)   -> 
      let h = Array.fold_left (fun h e -> combine h (e_hash e)) 0 es in
      combine h 6

  let hash e = hash_node e.e_node

end

module E = struct 

  module W = Weak.Make(HE)

  let id = ref 0 
  let h = W.create 5000

  let mk_expr e =
    let e = { e_id = -1; e_node = e } in
    try W.find h e 
    with Not_found ->
      let e = { e with e_id = !id } in
      W.add h e; incr id;
      e

  type t = expr

  let equal e1 e2 = e1 == e2

  let hash e = e.e_id

  let compare e1 e2 = e1.e_id - e2.e_id
    
end

module He = Hashtbl.Make(E)

module Se = Set.Make(E)

module Me = Map.Make(E)

let top       = E.mk_expr Etop
let rnd r     = E.mk_expr (Ernd r)
let share p i = E.mk_expr (Eshare(p,i))
let neg e     = E.mk_expr (Eneg e)
let add e1 e2 = E.mk_expr (Eadd(e1,e2))
let mul e1 e2 = E.mk_expr (Emul(e1,e2))

let unsafe_tuple b es = E.mk_expr (Etuple(b,es))

let tuple_nodup es = E.mk_expr (Etuple(true,es))

let tuple es  =
  let n = Array.length es in
  let tbl = He.create (2 * n) in
  let rec check i = 
    n <= i || 
      (not (He.mem tbl es.(i)) &&
         (He.add tbl es.(i) ();check (i+1))) in
  let b = check 0 in
  E.mk_expr (Etuple(b,es))
  
(* --------------------------------------------------------------------- *)
let pp_expr fmt e =
  let rec aux top fmt e = 
    match e.e_node with
    | Etop        -> Format.fprintf fmt "TOP"
    | Ernd r      -> pp_rnd fmt r
    | Eshare(p,i) -> pp_share fmt (p,i)
    | Eneg e      -> 
      Format.fprintf fmt "!%a" (aux `Not) e
    | Eadd(e1,e2) -> 
      begin match top with
      | `AddR | `MulL | `MulR | `Not ->
        Format.fprintf fmt "(%a + %a)" (aux `AddL) e1 (aux `AddR) e2
      | _ -> 
        Format.fprintf fmt "%a + %a" (aux `AddL) e1 (aux `AddR) e2
      end
    | Emul(e1,e2) -> 
      begin match top with
      | `MulR | `Not -> 
        Format.fprintf fmt "(%a.%a)" (aux `MulL) e1 (aux `MulR) e2
      | _ ->
        Format.fprintf fmt "%a.%a" (aux `MulL) e1 (aux `MulR) e2
      end 
    | Etuple(_, es) ->
      Format.fprintf fmt "(@[<hov 2>%a@])" 
         (pp_list ",@ " (aux `Top)) (Array.to_list es) in
  aux `Top fmt e
