open Util
open Poly
open Expr

module V = struct
  type t = expr 
  let equal = E.equal
  let compare = E.compare
              let pp = pp_expr
end

module P = Poly(V)

let to_pol tbl =
  let rec to_pol e = 
    try He.find tbl e 
    with Not_found ->
      let p = 
        match e.e_node with
        | Etop          -> assert false
        | Ernd _ | Eshare _ | Epub _ -> P.var e 
        | Eneg e        -> P.add P.one (to_pol e)
        | Eadd(e1,e2)   -> P.add (to_pol e1) (to_pol e2)
        | Emul(e1,e2)   -> P.mul (to_pol e1) (to_pol e2)
        | Eop(_, o, es) -> 
          if is_FF_op o then to_pol es.(1)
          else if E.equal e etrue then P.one 
          else if E.equal e efalse then P.zero
          else assert false in
      He.add tbl e p;
      p in
  to_pol

let rec split_tuple s e = 
  match e.e_node with
  | Eop(_, o, es) when is_op_tuple o ->
    Array.fold_left split_tuple s es 
  | _ -> e :: s 

exception Depend
exception Found of (expr * P.t)

let rnds excl p = 
  let tbl = He.create 101 in
  let add v = 
    if He.mem excl v then ()
    else if is_rnd v then He.add tbl v () in
  P.iter_vars add p;
  tbl
  
let find_rnd excl ps = 
  let todo = ref [] in
  let do1 p = 
(*    Format.eprintf "find_rnd in %a@." P.pp p; *)
    let rnds = rnds excl p in
(*    Format.eprintf "rnds ok@.";
    He.iter (fun r _ -> Format.eprintf "%a@." pp_expr r) rnds; *)
    let dor r _ = 
(*      Format.eprintf "test %a@." pp_expr r; *)
      let (p1, p2, _p3) = P.check_rnd_eucl r p in
(*      Format.eprintf "ICI1@."; *)
      (* p = (r + p1) * p2 + p3 *)
      if not (P.equal p1 P.zero) then
        if (P.equal p2 P.one) then raise (Found (r, p1))
        else todo := (r, p1) :: !todo in
    He.iter dor rnds in
  try List.iter do1 ps; !todo
  with Found rp -> [rp]
     | _ -> assert false

let subst x p p' = 
  let q, r = P.divx p' x in
  (* p' = q * x + r *)
  P.add (P.mul q p) r

let no_rnd = P.all_vars (fun e -> not (is_rnd e)) 

let initial_check_indep (continue:State.t_continue) ps = 

  let tbl = Hv.create 101 in
  let reset () = Hv.iter (fun _ pi -> State.Pinfo.clear pi) tbl in
  let get p = 
    try Hv.find tbl p 
    with Not_found -> State.Pinfo.empty () in
  let add p i = State.Pinfo.add_share i (get p) in

  let rec is_indep1 v = 
    match v.e_node with
    | Eshare(p,i,_) -> add p i
    | _ -> () in
  let is_indep depend ps = 
    reset ();
    List.iter (P.iter_vars is_indep1) depend; 
    List.iter (P.iter_vars is_indep1) ps; 
    not (continue tbl) in

  let rec aux excl depend ps = 
    Format.eprintf "aux@.";
    Format.eprintf "depend = @[<v>%a@]@." (pp_list "@ " P.pp) depend;
    Format.eprintf "ps = @[<v>%a@]@." (pp_list "@ " P.pp) ps;
    
    if is_indep depend ps then true
    else
      match find_rnd excl ps with
      | [] -> Format.eprintf "No rnd rule@."; false
      | (r,p) :: _ -> 
        Format.eprintf "r = %a; p = %a@." pp_expr r P.pp p;
        He.add excl r ();
        let ps = List.map (subst r (P.add (P.var r) p)) ps in
        let dps, ps = List.partition no_rnd ps in
        aux excl (List.rev_append dps depend) ps in

  aux (He.create 101) [] ps

let check_indep (continue:State.t_continue) (es:expr list) =

  let pol_tbl = He.create 101 in
  let to_pol = to_pol pol_tbl in
  let ps = 
    let to_pols s e = 
      let es = split_tuple [] e in
      List.fold_left (fun s e -> to_pol e :: s) s es in
    List.fold_left to_pols [] es in
  
  initial_check_indep continue ps
