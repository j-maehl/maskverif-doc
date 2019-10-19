open Util
open Expr

(* ----------------------------------------------------------------------- *)

type node = {
          node_id    : int;            (* uniq id for the node *)
  mutable children   : node Vector.t;  (* List of nodes directly using it *)
  mutable descriptor : descriptor;     (* The current value of the node *)
          expr_desc  : expr;           (* Current expression representing
                                          the node, and its original version *)
  mutable class_size  : int;           (* the size of the class *)
                                       (*   < 0 means the class is not computed *)
                                       (*   = 0 means the node is a public term *)
  mutable class_parent : class_parent;
}

and class_parent = 
  | CPnone 
  | CPsome of {mutable parent : node; }

and descriptor =
| Top
| Rnd   of rnd
| Share of param * int * var
| Pub   of var
| Priv  of var 
| Const of constant
| Op1   of operator * node 
| Op2   of operator * node * node 
| Tuple of bool * operator * node array
  (* Invariant [Tuple(b,o,ns)]
       if [b] is true there is no duplicate in the array [ns] *)

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
    | Op1(op, _) -> op.op_bij = Bij
    | Op2(op, n1, n2) -> 
      begin match op.op_bij with 
      | Bij -> not (N.equal n1 n2)
      | NotBij -> false 
      | PartialBij bij -> 
        not (N.equal n1 n2) && 
          ((bij.(0) && N.equal n n1) ||
           (bij.(0) && N.equal n n2))
      end
    | Tuple(true, op, args) -> 
      begin match op.op_bij with 
      | Bij -> true 
      | NotBij -> false 
      | PartialBij bij -> 
        Array.exists2 (fun b n1 -> b && N.equal n n1) bij args
      end
    | Tuple(false, op, ns) -> 
      begin match op.op_bij with
      | Bij -> 
        let res = ref 0 in
        Array.iter (fun n' -> if N.equal n n' then incr res) ns;
        !res = 1
      | NotBij -> false
      | PartialBij bij ->
        let res = ref 0 in
        for i = 0 to Array.length ns - 1 do
          if N.equal n ns.(i) then 
            if bij.(i) then incr res else res := 2
        done;
        !res = 1
      end
    | _ -> false

let is_share node =
  match node.descriptor with
  | Share _ -> true
  | _       -> false

let top_node = {
  node_id = -1;
  children = Vector.dummy ();
  descriptor = Top;
  expr_desc  = top;
  class_size = -1;
  class_parent = CPnone;
}

(* ----------------------------------------------------------------------- *)

let pp_descriptor fmt = function
  | Top            -> Format.fprintf fmt "TOP"
  | Rnd i          -> pp_rnd fmt i
  | Share (p,i,v)  -> pp_share fmt (p,i,v)
  | Pub x          -> pp_var fmt x
  | Priv x         -> pp_var fmt x
  | Const c        -> C.pp fmt c
  | Op1(op, e)     -> Format.fprintf fmt "%s %i" op.op_name e.node_id
  | Op2(op, e1,e2) -> Format.fprintf fmt "%i %s %i" e1.node_id op.op_name e2.node_id
  | Tuple(_,o,es)->
    Format.fprintf fmt "%s(@[%a@])" o.op_name
      (pp_list "@, "(fun fmt e -> Format.fprintf fmt "%i" e.node_id))
      (Array.to_list es)


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

  let init () = ref (0)  (* Do not change this *)

  let reset c = c := 0   (* Do not change this *)

  let next c = incr c; !c

end

(* ----------------------------------------------------------------------- *)

module Pinfo = struct

  type t = {
    mutable nb_used_shares : int; 
    mutable used_shares    : SmallSet.t;
  }

  let empty () = {
      nb_used_shares = 0;
      used_shares    = SmallSet.empty;
    }

  let copy pi = { nb_used_shares = pi.nb_used_shares; used_shares = pi.used_shares }

  let add_share i info = 
    if (not (SmallSet.mem i info.used_shares)) then begin 
        info.nb_used_shares <- info.nb_used_shares + 1;
        info.used_shares <- SmallSet.add info.used_shares i
      end

  let remove_share i info = 
    if (SmallSet.mem i info.used_shares) then begin
        info.nb_used_shares <- info.nb_used_shares - 1;
        info.used_shares <- SmallSet.remove info.used_shares i
      end

  let iter f info = SmallSet.iter f info.used_shares

  let clear info =
    info.nb_used_shares <- 0;
    info.used_shares <- SmallSet.empty

end

(* ----------------------------------------------------------------------- *)

type state = {
    s_nb_shares: int;
    s_count    : Count.t;
    s_hash     : node He.t;
    s_params   : Pinfo.t Hv.t;
    s_shares   : node array Hv.t;
    s_priv     : node Stack.t;
    s_randoms  : node Stack.t;  (* random nodes *)
    s_todo     : node Stack.t;  (* next random to eliminate *)
    s_top      : node Vector.t; (* parents of top *)
    s_bij      : (node*node) Stack.t;
    s_params_c : node Hv.t;   
  }


let init_state nb_shares params =

  let s_count = Count.init () in

  (* Create the node corresponding to each share *)
  let nb_params  =  List.length params in
  let s_shares   = Hv.create nb_params in
  let s_params   = Hv.create nb_params in
  let s_params_c = Hv.create nb_params in

  List.iter (fun p -> 
    let p_info = Pinfo.empty () in
    let a = Array.make nb_shares top_node in
    Hv.add s_params p p_info;
    Hv.add s_shares p a) params;

  (* Add top to the hash table *)
  let s_hash = He.create 100 in
  He.add s_hash top top_node;

  (* Build the final state *)
  { s_nb_shares = nb_shares;
    s_count;
    s_hash;
    s_params;
    s_shares;
    s_params_c;
    s_priv    = Stack.make 1000 top_node;
    s_randoms = Stack.make 1000 top_node;
    s_todo = Stack.make 1000 top_node;
    s_top  = Vector.create 1000 top_node;
    s_bij  = Stack.make 100 (top_node, top_node);
  }

let clear_state state =
  Count.reset state.s_count;
  He.clear state.s_hash;
  Hv.iter (fun _ info -> Pinfo.clear info) state.s_params;
  Hv.clear state.s_params_c;
  Stack.clear state.s_priv;
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
      Format.fprintf fmt "%a:%i;" 
         pp_var p 
         pinfo.Pinfo.nb_used_shares;
      SmallSet.iter (fun i -> Format.fprintf fmt " %i" i) pinfo.Pinfo.used_shares;
      Format.fprintf fmt "@.") state.s_params;
  Format.fprintf fmt "@]";
  Format.fprintf fmt "@[<h>bijection = %a@]@ "
    (pp_list "@ "
        (fun fmt (r,c) ->
          Format.fprintf fmt "(%a,%a)"
             pp_node_id r pp_node_id c)) (Stack.to_list state.s_bij);
  Format.fprintf fmt "@]"

(* ----------------------------------------------------------------------- *)

let add_used_share state p i = 
  Pinfo.add_share i (Hv.find state.s_params p) 

let rm_used_share state p i = 
  Pinfo.remove_share i (Hv.find state.s_params p) 

let declare_share state p i n = 
  let a = Hv.find state.s_shares p in
  a.(i) <- n

let declare_random state n =
  Stack.push state.s_randoms n

let declare_priv state n = 
  Stack.push state.s_priv n

let add_children state p c =
  begin match p.descriptor with
  | Share(x,i,_) when Vector.size p.children = 0 ->
    add_used_share state x i
  | _ -> ()
  end;
  Vector.push p.children c

let set_parents state n =
  match n.descriptor with
  | Rnd _ | Share _ | Pub _ | Priv _ | Top | Const _ -> ()
  | Op1(_, p)   -> add_children state p n
  | Op2(_,p1,p2) -> 
    add_children state p1 n;
    if not (N.equal p1 p2) then add_children state p2 n
  | Tuple(b, _o, ps) ->
    if b then (* No duplicate *)
      for i = 0 to Array.length ps - 1 do
        add_children state ps.(i) n
      done
    else
      let tbl = Hn.create (2 * Array.length ps) in
      for i = 0 to Array.length ps - 1 do
        let p = ps.(i) in
        if not (Hn.mem tbl p) then
          (Hn.add tbl p (); add_children state p n)
      done

let rec add_expr state e =
  try He.find state.s_hash e
  with Not_found ->
    let descriptor =
      match e.e_node with
      | Etop          -> assert false
      | Ernd r        -> Rnd r
      | Eshare(p,i,v) -> Share(p,i,v)
      | Epub x        -> Pub x
      | Epriv x       -> Priv x
      | Econst c      -> Const c
      | Eop1(o,e)     ->
        let n = add_expr state e in
        Op1(o, n)
      | Eop2(o,e1,e2)  ->
        let n1 = add_expr state e1 in
        let n2 = add_expr state e2 in
        Op2(o,n1,n2)
      | Eop(b, o, es) ->
        let ns = Array.map (add_expr state) es in
        Tuple(b, o, ns)
    in
    let n =
      { node_id      = Count.next state.s_count;
        children     = Vector.create 3 top_node;
        descriptor   = descriptor;
        expr_desc    = e;
        class_parent = CPnone;
        class_size   = -1;
      } in
    (* add the node to the parents *)
    set_parents state n;
    He.add state.s_hash e n;
    begin match descriptor with
    | Rnd _        -> declare_random state n
    | Share(p,i,_) -> declare_share state p i n
    | Priv _       -> declare_priv state n        
    | _            -> ()
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
  | Share(a,i,_) -> rm_used_share state a i 
  | Pub _ -> ()
  | Priv _ -> ()
  | Const _ -> ()
  | Op1(_,p) -> remove_child state p n
  | Op2(_,p1,p2) ->
    remove_child state p1 n;
    remove_child state p2 n
  | Tuple(_, _o, ps) ->
    for i = 0 to Array.length ps - 1 do
      remove_child state ps.(i) n
    done

let remove_other_parent state p c =
  match c.descriptor with
  | Op1 _ -> ()
  | Op2(_,p1, p2) ->
    assert (N.equal p p1 || N.equal p p2);
    if not (N.equal p p1) then remove_child state p1 c;
    if not (N.equal p p2) then remove_child state p2 c
  | Tuple(_, _, ps) ->
    let found = ref false in
    Array.iter (fun pi -> 
        if N.equal p pi then found := true
        else remove_child state pi c) ps
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

exception Simplify1Done

let rec simplify1 state =
  try
    while true do
      let r = Stack.pop state.s_todo in
      if is_rnd_for_bij r then (apply_bij state r; raise Simplify1Done)
    done;
    true
  with EmptyStack -> false
     | Simplify1Done -> true

let simplify state =
  let rec aux state res =
    if simplify1 state then aux state true
    else res in
  aux state false

exception FoundShare of Pinfo.t

let find_share k sparams = 
  Hv.iter 
    (fun _p pinfo -> 
      if k < pinfo.Pinfo.nb_used_shares then raise (FoundShare pinfo))
    sparams

let continue_k k sparams = 
  try find_share k sparams; false
  with FoundShare _ -> true

let continue_spini k sparams = 
  if continue_k k sparams then true
  else 
    let u = 
      Hv.fold (fun _p pinfo s -> SmallSet.union pinfo.Pinfo.used_shares s)  sparams SmallSet.empty in
    k < SmallSet.card u
    
type t_continue = Pinfo.t Hv.t -> bool

let check_priv state = 
  try 
    Stack.iter (fun n -> 
        if Vector.size n.children <> 0 then raise Not_found) state.s_priv;
    false
  with Not_found ->
    true

let rec simplify_until (continue: t_continue) state = 
  let c = continue state.s_params || check_priv state in
  if c then
    let b = simplify state in
    if b then simplify_until continue state 
    else false
  else true
(*
  while continue state && simplify state do () done;
  not (continue state)
*)



(* ----------------------------------------------------------------------- *)

let set_top_node state n =
  Vector.push state.s_top n;
  add_children state n top_node

let add_top_expr state e =
  let n = add_expr state e in
  set_top_node state n;
  n

(* ----------------------------------------------------------------------- *)

let simplified_expr ?(notfound=false) state =
  let tbl = Hn.create 1000 in
  let rec aux n =
    try Hn.find tbl n
    with Not_found ->
      let e =
        match n.descriptor with
        | Top        -> top
        | Rnd r      -> rnd r
        | Share(p,i,v) -> share p i v
        | Pub x      -> pub x
        | Priv x     -> priv x
        | Const c    -> econst c
        | Op1(o,n)   -> op1 o (aux n)
        | Op2(o, n1,n2) -> op2 o (aux n1) (aux n2)
        | Tuple(b, o, ns) -> unsafe_op b o (Array.map aux ns)
      in
      Hn.add tbl n e;
      e in
  let doit e =
    try aux (He.find state.s_hash e)
    with Not_found ->
      assert (notfound); e in
  doit


(* ----------------------------------------------------------------------- *)

let rec remove_node_and_all_children state n =
(*  Format.eprintf "remove_node %a@." pp_node n; *)
  if not (N.equal n top_node) then begin
    while (Vector.size n.children <> 0) do
      let c = Vector.pop n.children in
      remove_node_and_all_children state c
    done;
    remove_node state n;
  end


let clear_bijection state =
  Stack.iter (fun (n,_) -> remove_node_and_all_children state n) state.s_bij

(* ----------------------------------------------------------------------- *)

let used_share = 
  let tbl = Hv.create 100 in
  fun state -> 
    Hv.clear tbl;
    Hv.iter (fun p pi -> Hv.add tbl p (Pinfo.copy pi)) state.s_params;
    fun p i -> 
    try SmallSet.mem i (Hv.find tbl p).Pinfo.used_shares 
    with Not_found -> false 

(* ----------------------------------------------------------------------- *)

exception Found of node

let find_used_share_except state excepted =
  try
    Hv.iter (fun p pinfo -> 
      Pinfo.iter (fun i -> 
          if not (excepted p i) then
            let na = (Hv.find state.s_shares p).(i) in
            raise (Found na)) pinfo) state.s_params;
    assert false
  with Found n -> n

let remove_used_share_except state excepted =
  let na = find_used_share_except state excepted in
  remove_node_and_all_children state na


let rec simplify_until_with_clear (continue:t_continue) state excepted = 
  while continue state.s_params do 
    if not (simplify state) then 
      remove_used_share_except state excepted;
  done

(* ----------------------------------------------------------------------- *)

let is_top_expr state e =
  let n =
    try He.find state.s_hash e
    with Not_found -> assert false in
  Vector.exists (N.equal top_node) n.children

(* ------------------------------------------------------------------------ *)

type bijection =  (expr * expr) Stack.t

let get_bij state =
  Stack.map (fun (n1,n2) -> n1.expr_desc, n2.expr_desc) state.s_bij

let get_expr state e =
  try He.find state.s_hash e with Not_found -> assert false

let replay_bij1 state (e1, e2) =
  let n1 = get_expr state e1 in
  let n2 = get_expr state e2 in
  (* remove_node_and_all_children, excepted n2 *)
  if Vector.size n1.children <> 1 then
    begin
      let cs = Vector.copy n1.children in
      Vector.iter (fun n ->
          if not (N.equal n n2) then
            begin
              remove_node_and_all_children state n;
              remove_child state n1 n
            end) cs
    end;
  (* We check that the bijection can be applied *)
  assert (N.equal (Vector.top n1.children) n2);
  apply_bij state n1

let replay_bij state bij =
  Stack.iter (replay_bij1 state) bij

(* --------------------------------------------------------------------- *)

let mem_class n = 0 <= n.class_size

let rec find_class n = 
  assert (mem_class n);
  match n.class_parent with
  | CPnone -> n
  | CPsome p ->
    let np = p.parent in
    let np' = find_class np in
    if not (N.equal np np') then p.parent <- np';
    np' 

let union_class n1 n2 = 
   assert (mem_class n1 && mem_class n2);
   let p1 = find_class n1 in
   let p2 = find_class n2 in
   if not (N.equal p1 p2) then
     let s1 = p1.class_size in
     let s2 = p2.class_size in
     let p1, p2 = 
       if s2 < s1 then p2, p1 else p1, p2 in
     p1.class_parent <- CPsome {parent = p2};
     p2.class_size   <- s1 + s2

let init_class state e = 

  let ptbl = state.s_params_c in
  let add_share p n = 
    match Hv.find_opt ptbl p with
    | Some n' -> union_class n n' 
    | None    -> Hv.add ptbl p n in 

  let all_pub = ref true in

  let is_pub n = n.class_size = 0 in

  let add_sub n n1 = 
    if not (is_pub n1) then
      (all_pub := false; union_class n n1) in

  let rec init_class n = 
    if n.class_size < 0 then 
      match n.descriptor with
      | Top             -> assert false
      | Rnd _ | Priv _  -> n.class_size <- 1
      | Pub _ | Const _ -> n.class_size <- 0
      | Share (p,_,_)   -> 
        n.class_size <- 1;
        add_share p n
      | Op1(_, n1) ->
        init_class n1;
        if is_pub n1 then n.class_size <- 0
        else (n.class_size <- 1; union_class n n1)
      | Op2(_,n1,n2) ->
        init_class n1; init_class n2;
        n.class_size <- 1;
        all_pub := true;
        add_sub n n1; add_sub n n2;
        if !all_pub then n.class_size <- 0;
      | Tuple (_, _, ns) ->
        Array.iter init_class ns;
        n.class_size <- 1;
        all_pub := true;
        Array.iter (add_sub n) ns;
        if !all_pub then n.class_size <- 0 in

  init_class (add_expr state e)

let get_class state e = 
  let n = find_class (add_expr state e) in
  assert (mem_class n);
  if n.class_size = 0 then 0
  else n.node_id


        
