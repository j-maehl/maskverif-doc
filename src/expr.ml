type var = {
  v_id   : int;
  v_name : string;
}

module V = struct 

  let id = ref 0 

  let tbl = Hashtbl.create 100 

  let mk_var s =
    try Hashtbl.find tbl s 
    with Not_found ->
      let p = { v_id = !id; v_name = s } in
      incr id;
      Hashtbl.add tbl s p;
      p

  type t = var 

  let equal v1 v2 = v1 == v2

  let hash v = v.v_id

  let compare v1 v2 = v1.v_id - v2.v_id
    
end

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
| Eadd   of expr * expr
| Emul   of expr * expr

let e_equal e1 e2 = e1 == e2
let e_hash e = e.e_id

module HE = struct

  type t = expr

  let equal_node n1 n2 = 
    match n1, n2 with 
    | Etop         , Etop          -> true
    | Ernd r1      , Ernd r2       -> V.equal r1 r2
    | Eshare(p1,i1), Eshare(p2,i2) -> i1 == i2 && V.equal p1 p2
    | Eadd(e11,e12), Eadd(e21,e22) -> e_equal e11 e21 && e_equal e12 e22
    | Emul(e11,e12), Emul(e21,e22) -> e_equal e11 e21 && e_equal e12 e22
    | _                            -> false

  let equal e1 e2 = equal_node e1.e_node e2.e_node 

  let hash_node = function
    | Etop        -> 1
    | Ernd r      -> V.hash r
    | Eshare(p,i) -> (V.hash p lsl 8) + i
    | Eadd(e1,e2) -> (e_hash e1 lsl 8) + (e_hash e2 lsl 1) 
    | Emul(e1,e2) -> (e_hash e1 lsl 8) + (e_hash e2 lsl 1) + 1

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
let add e1 e2 = E.mk_expr (Eadd(e1,e2))
let mul e1 e2 = E.mk_expr (Emul(e1,e2))

let pp_expr fmt e =
  let rec aux top fmt e = 
    match e.e_node with
    | Etop        -> Format.fprintf fmt "TOP"
    | Ernd r      -> pp_rnd fmt r
    | Eshare(p,i) -> pp_share fmt (p,i)
    | Eadd(e1,e2) -> 
      begin match top with
      | `AddR | `MulL | `MulR ->
        Format.fprintf fmt "(%a + %a)" (aux `AddL) e1 (aux `AddR) e2
      | _ -> 
        Format.fprintf fmt "%a + %a" (aux `AddL) e1 (aux `AddR) e2
      end
    | Emul(e1,e2) -> 
      begin match top with
      | `MulR -> 
        Format.fprintf fmt "(%a.%a)" (aux `MulL) e1 (aux `MulR) e2
      | _ ->
        Format.fprintf fmt "%a.%a" (aux `MulL) e1 (aux `MulR) e2
      end in
  aux `Top fmt e
