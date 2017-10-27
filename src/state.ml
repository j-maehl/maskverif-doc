open Util 
open Expr
   
(* ----------------------------------------------------------------------- *)
    
type node = { 
          node_id    : int;            (* uniq id for the node *)
  mutable children   : node Vector.t;  (* List of nodes directly using it *)
  mutable descriptor : descriptor;     (* The current value of the node *)
          expr_desc  : expr;           (* Current expression representing 
                                          the node, and its original version *)
}

and descriptor = 
| Top
| Rnd   of rnd 
| Share of param * int 
| Add   of node * node
| Mul   of node * node

module N = struct 
  type t = node
  let equal (n1:t) (n2:t) = n1.node_id == n2.node_id
  let hash n = n.node_id
end

module Hn = Hashtbl.Make(N)

let is_rnd node = 
  match node.descriptor with 
  | Rnd _ -> true 
  | _     -> false 

let is_rnd_for_bij n = 
  Vector.size n.children = 1 && is_rnd n &&
    let c = Vector.top n.children in
    match c.descriptor with
    | Add(n1,n2) -> not (N.equal n1 n2)
    | _          -> false

let is_share node = 
  match node.descriptor with 
  | Share _ -> true 
  | _       -> false 

let top_node = {
  node_id = -1;
  children = Vector.dummy ();
  descriptor = Top;
  expr_desc  = top;
}

(* ----------------------------------------------------------------------- *)

let pp_descriptor fmt = function 
  | Top        -> Format.fprintf fmt "TOP"
  | Rnd i      -> pp_rnd fmt i
  | Share (p,i)-> pp_share fmt (p,i)
  | Add(e1,e2) -> Format.fprintf fmt "%i + %i" e1.node_id e2.node_id
  | Mul(e1,e2) -> Format.fprintf fmt "%i * %i" e1.node_id e2.node_id

let pp_children fmt children = 
  Vector.iter (fun c -> Format.fprintf fmt "%i;" c.node_id) children

let pp_node fmt node = 
  Format.fprintf fmt "%i:[%a] -> %a   // @[<h>%a@]@ " 
    node.node_id 
    pp_descriptor node.descriptor
    pp_children node.children
    pp_expr node.expr_desc

 
(* ----------------------------------------------------------------------- *)

module Count = struct

  type t = int ref

  let init () = ref (-1)

  let reset c = c := -1 

  let next c = incr c; !c

end

(* ----------------------------------------------------------------------- *)

module Pinfo = struct

  type t = {
    mutable nb_used_shares : int; 
            p_shares       : node Stack.t;
  }

  let init nb_params = {
    nb_used_shares = 0;
    p_shares = Stack.make nb_params top_node;
  }

  let declare info share =
    Stack.push info.p_shares share

  let incr info = 
    info.nb_used_shares <- info.nb_used_shares + 1

  let decr info = 
    info.nb_used_shares <- info.nb_used_shares - 1

  let iter f info = Stack.iter f info.p_shares

  let clear info = 
    info.nb_used_shares <- 0;
    Stack.clear info.p_shares

end

(* ----------------------------------------------------------------------- *)

type state = {
    s_nb_shares: int;
    s_count    : Count.t;
    s_hash     : node He.t;
    s_params   : Pinfo.t Hv.t;
    s_randoms  : node Stack.t;  (* random nodes *)
    s_todo     : node Stack.t;  (* next random to eliminate *)
    s_top      : node Vector.t; (* parents of top *)
    s_bij      : (node*node) Stack.t;
  }


let init_state nb_shares params =

  let s_count = Count.init () in

  (* Create the node corresponding to each share *)
  let s_params = Hv.create (2 * List.length params) in
  List.iter (fun p -> 
    let p_info = Pinfo.init nb_shares in
    Hv.add s_params p p_info) params;

  (* Add top to the hash table *)
  let s_hash = He.create 1000 in
  He.add s_hash top top_node;

  (* Build the final state *)
  { s_nb_shares = nb_shares;
    s_count;
    s_hash;
    s_params;
    s_randoms = Stack.make 1000 top_node;
    s_todo = Stack.make 1000 top_node;
    s_top  = Vector.create 1000 top_node;
    s_bij  = Stack.make 100 (top_node, top_node);
  }

let clear_state state = 
  Count.reset state.s_count;
  He.clear state.s_hash;
  Hv.iter (fun _ info -> Pinfo.clear info) state.s_params;
  Stack.clear state.s_randoms;
  Stack.clear state.s_todo;
  Vector.clear state.s_top;
  Stack.clear state.s_bij

let get_top state = 
  let ns = Vector.to_list state.s_top in
  List.map (fun n -> n.expr_desc) ns

(* ----------------------------------------------------------------------- *)

let pp_node_id fmt n = Format.fprintf fmt "%i" n.node_id

let pp_state fmt state =
  Format.fprintf fmt "@[<v>";
  He.iter (fun _ n -> pp_node fmt n) state.s_hash;
  Format.fprintf fmt "@[<h>randoms = %a@]@ "
    (pp_list "@ " pp_node_id) (Stack.to_list state.s_randoms);
  Format.fprintf fmt "@[<h>todo = %a@]@ "
    (pp_list "@ " pp_node_id) (Stack.to_list state.s_todo);
  Format.fprintf fmt "@[<h>top = %a@]@ "
    (pp_list "@ " pp_node_id) (Vector.to_list state.s_top);
  Format.fprintf fmt "@[<v> params = ";
  Hv.iter (fun p pinfo ->
      Format.fprintf fmt "%a:%i;@ " 
         pp_var p 
         pinfo.Pinfo.nb_used_shares) state.s_params;
  Format.fprintf fmt "@]";
  Format.fprintf fmt "@[<h>bijection = %a@]@ "
    (pp_list "@ " 
        (fun fmt (r,c) -> 
          Format.fprintf fmt "(%a,%a)"
             pp_node_id r pp_node_id c)) (Stack.to_list state.s_bij);
  Format.fprintf fmt "@]"
  
(* ----------------------------------------------------------------------- *)  

let add_used_share state p = 
  Pinfo.incr (Hv.find state.s_params p) 

let rm_used_share state p = 
  Pinfo.decr (Hv.find state.s_params p)

let declare_share state p n = 
  Pinfo.declare (Hv.find state.s_params p) n

let declare_random state n = 
  Stack.push state.s_randoms n

let add_children state p c = 
  begin match p.descriptor with
  | Share(x,_) when Vector.size p.children = 0 ->
    add_used_share state x
  | _ -> ()
  end;
  Vector.push p.children c
  
let set_parents state n = 
  match n.descriptor with
  | Rnd _   -> ()
  | Share _ -> ()
  | Top     -> ()
  | Add(p1,p2) | Mul(p1,p2) -> 
    add_children state p1 n;
    if not (N.equal p1 p2) then add_children state p2 n

let rec add_expr state e = 
  try He.find state.s_hash e 
  with Not_found ->
    let descriptor = 
      match e.e_node with
      | Etop         -> assert false 
      | Ernd r       -> Rnd r
      | Eshare(p, i) -> Share(p,i)
      | Eadd(e1,e2)  ->
        let n1 = add_expr state e1 in
        let n2 = add_expr state e2 in
        Add(n1,n2)
      | Emul(e1,e2)  ->
        let n1 = add_expr state e1 in
        let n2 = add_expr state e2 in
        Mul(n1,n2)
    in
    let n = 
      { node_id    = Count.next state.s_count;
        children   = Vector.create 3 top_node;
        descriptor = descriptor;
        expr_desc  = e; } in
    (* add the node to the parents *) 
    set_parents state n;
    He.add state.s_hash e n;
    begin match descriptor with
    | Rnd _      -> declare_random state n
    | Share(p,_) -> declare_share state p n
    | _           -> ()
    end;
    n

(* ----------------------------------------------------------------------- *)

let init_todo state = 
  Stack.iter (fun n -> 
      if Vector.size n.children = 1 then Stack.push state.s_todo n) 
             state.s_randoms

(* ----------------------------------------------------------------------- *)

let rec remove_child state p c =
  Vector.remove (N.equal c) p.children;
  match Vector.size p.children with
  | 0 -> remove_node state p
  | 1 when is_rnd p -> Stack.push state.s_todo p
  | _ -> ()

and remove_node state n = 
  assert (Vector.size n.children = 0);
  match n.descriptor with
  | Top   -> assert false 
  | Rnd _ -> ()
  | Share(a,_) -> rm_used_share state a 
  | Add(p1,p2) | Mul(p1,p2) ->
    remove_child state p1 n;
    remove_child state p2 n

let remove_other_parent state p c = 
  match c.descriptor with
  | (Add(p1,p2) | Mul(p1,p2))  when (N.equal p p1 || N.equal p p2) ->
    if not (N.equal p p1) then remove_child state p1 c;
    if not (N.equal p p2) then remove_child state p2 c
  | _ -> assert false 


let apply_bij state r = 
  assert (is_rnd_for_bij r);
  let c = Vector.pop r.children in
  remove_other_parent state r c;
  c.descriptor <- r.descriptor;
  (* c become a random so push it on s_random *)
  Stack.push state.s_randoms c;
  Stack.push state.s_bij (r,c);
  if (is_rnd_for_bij c) then Stack.push state.s_todo c
                    
let rec simplify1 state = 
  try 
    let r = Stack.pop state.s_todo in
    if is_rnd_for_bij r then (apply_bij state r; true)
    else simplify1 state
  with EmptyStack -> false 

let simplify state = 
  let rec aux state res = 
    if simplify1 state then aux state true
    else res in
  aux state false 

exception FoundShare of Pinfo.t

let find_share state k = 
  Hv.iter 
    (fun _p pinfo -> if k < pinfo.Pinfo.nb_used_shares then raise (FoundShare pinfo))
    state.s_params

let continue state k = 
  try find_share state k; false
  with FoundShare _ -> true

let simplify_until state k = 
  while continue state k && simplify state do () done;
  not (continue state k)



  
(* ----------------------------------------------------------------------- *)

let add_top_expr state e = 
  let n = add_expr state e in
  Vector.push state.s_top n;
  add_children state n top_node;
  n

(*
let remove_top state n = 
  Vector.remove (N.equal n) state.s_top;
  remove_child state n top_node
*)

(* ----------------------------------------------------------------------- *)

let simplified_expr state = 
  let tbl = Hn.create 1000 in
  let rec aux n = 
    try Hn.find tbl n 
    with Not_found ->
      let e = 
        match n.descriptor with
        | Top        -> top
        | Rnd r      -> rnd r
        | Share(p,i) -> share p i
        | Add(n1,n2) -> add (aux n1) (aux n2)
        | Mul(n1,n2) -> mul (aux n1) (aux n2) in
      Hn.add tbl n e;
      e in
  let doit e = 
    let n = 
      try He.find state.s_hash e 
      with Not_found -> assert false in
    aux n
  in
  doit 


(* ----------------------------------------------------------------------- *)

let rec remove_node_and_all_children state n = 
  if not (N.equal n top_node) then begin 
    Vector.iter (fun c -> remove_node_and_all_children state c) n.children;
    Vector.clear n.children;
    remove_node state n
  end
    
  
let clear_bijection state = 
  Stack.iter (fun (n,_) -> remove_node_and_all_children state n) state.s_bij
  
(* ----------------------------------------------------------------------- *)

let used_share = 
  let hn = Hn.create 100 in
  fun state -> 
  Hn.clear hn;
  Hv.iter 
    (fun _ pinfo -> 
      Pinfo.iter 
        (fun na -> if Vector.size na.children <> 0 then Hn.add hn na ()) 
        pinfo)
    state.s_params;
  fun n -> Hn.mem hn n

(* ----------------------------------------------------------------------- *)

exception Found of node 

let find_used_share_except state excepted = 
  try
    Hv.iter (fun _ pinfo -> 
      Pinfo.iter (fun na -> 
          if Vector.size na.children <> 0 && not (excepted na) then
            raise (Found na)) pinfo) state.s_params;
    assert false
  with Found n -> n
    
let remove_used_share_except state excepted =
  let na = find_used_share_except state excepted in
  remove_node_and_all_children state na


let simplify_until_with_clear state excepted k = 
  while continue state k do 
    if not (simplify state) then 
      remove_used_share_except state excepted 
  done

let find_used_share info = 
  try 
    Pinfo.iter (fun na -> 
        if Vector.size na.children <> 0 then raise (Found na)) info;
    assert false
  with Found n -> n

let find_top n = 
  let rec aux n = 
    let has = ref false in
    let doit c = 
      if N.equal c top_node then has := true
      else aux c in
    Vector.iter doit n.children;
    if !has then raise (Found n) in
  try aux n; assert false
  with Found n -> n

(*
exception CanNotCheck of expr list

type removable = {
   nb_to_keep : int;
   removable  : node Vector.t  
}

let dummy_removable = 
  { nb_to_keep = 0;
    removable = Vector.create 0 top_node; }

let _ = Random.self_init ()

let simplify_until_with_clear2 state k can_remove = 
  let len = state.s_nb_shares in 
  let cr = Vector.create len dummy_removable in
  let add (k, ees, len) =
    let v = Vector.create len top_node in
    let add_node (e1,_) = Vector.push v (add_expr state e1) in
    List.iter add_node ees;
    let rm = { nb_to_keep = k; removable = v } in
    Vector.push cr rm in
  List.iter add can_remove;
  let select_top () = 
    let cr_size = Vector.size cr in
    if cr_size = 0 then raise (CanNotCheck (get_top state));
    let k = Random.int cr_size in
    let rm = Vector.get cr k in
    let size = Vector.size rm.removable in
    let i = Random.int size in
    let n = Vector.get rm.removable i in
    if size - 1 <= rm.nb_to_keep then Vector.unset cr k 
    else Vector.unset rm.removable i;
    n in
  let rec aux () = 
    if not (simplify_until state k) then
      let ti = select_top () in
      remove_node_and_all_children state ti;
      Format.eprintf "remove %a@." pp_expr ti.expr_desc;
      Vector.remove (N.equal ti) state.s_top;
      aux () in
  aux ()

 *)


(* ----------------------------------------------------------------------- *)

let is_top_expr state e = 
  let n = 
    try He.find state.s_hash e 
    with Not_found -> assert false in
  Vector.exists (N.equal top_node) n.children
