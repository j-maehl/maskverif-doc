open Util
open Expr

let with_clear = ref false

type p_expr =
  | PEvar of expr
  | PEconst of bool
  | PEadd of p_expr list
  | PEmul of p_expr list

let rec pp_p_expr ty fmt pe =
  match pe with
  | PEvar e  ->
    Format.fprintf fmt "@[(%a)@]" pp_expr e
  | PEconst b -> 
    Format.fprintf fmt "@[(%a)@]" pp_expr (if b then eone ty else ezero ty)
  | PEadd ls -> 
    let s = (o_add ty).op_name in
    let sep = Scanf.format_from_string (" "^s^"@ ") "" in
    Format.fprintf fmt "@[(%a)@]" (pp_list sep (pp_p_expr ty)) ls
  | PEmul ls -> 
    let s = (o_mul ty).op_name in
    let sep = Scanf.format_from_string (" "^s^"@ ") "" in
    Format.fprintf fmt "@[(%a)@]" (pp_list sep (pp_p_expr ty)) ls

let zero = PEconst false 
let one  = PEconst true 

let is_zero p =
  match p with
  | PEconst b -> not b
  | _ -> false

let is_one p =
  match p with
  | PEconst b -> b 
  | _ -> false

let rec compare_p p1 p2 =
  match p1, p2 with
  | PEconst b1, PEconst b2 -> compare b1 b2
  | PEconst _, _           -> -1
  | _, PEconst _           -> 1
  | PEvar x1, PEvar x2   -> E.compare x1 x2
  | PEvar _, _           -> -1
  | _, PEvar _           -> 1
  | PEadd ps1, PEadd ps2 -> compare_s ps1 ps2
  | PEadd _, _           -> -1
  | _, PEadd _           -> 1
  | PEmul ps1, PEmul ps2 -> compare_s ps1 ps2

and compare_s ps1 ps2 =
  match ps1, ps2 with
  | [], [] -> 0
  | [], _  -> -1
  | _, []  -> 1
  | p1::ps1, p2::ps2 ->
    match compare_p p1 p2 with
    | 0 -> compare_s ps1 ps2
    | c -> c

module Pcmp =
  struct
    type t = p_expr
    let compare = compare_p
  end
module Sp = Set.Make(Pcmp)

(* ----------------------------------------------------- *)
(* Addition                                              *)

let add_to_list p =
  match p with
  | PEadd ps -> ps
  | _        -> [p]

let rec merge_add ps1 ps2 =
  match ps1, ps2 with
  | [], _ -> ps2
  | _, [] -> ps1
  | p1::ps1', p2::ps2' ->
    match compare_p p1 p2 with
    | 0            -> merge_add ps1' ps2'
    | c when c < 0 -> p1 :: merge_add ps1' ps2
    | _            -> p2 :: merge_add ps1  ps2'

let mk_add ps =
  match ps with
  | []  -> zero
  | [p] -> p
  | _   -> PEadd ps

let add p1 p2 =
  if is_zero p1 then p2
  else if is_zero p2 then p1
  else
  let ps1 = add_to_list p1 in
  let ps2 = add_to_list p2 in
  mk_add (merge_add ps1 ps2)

(* Simplify p + !p into 1 *)
let clear_add p =
  match p with
  | PEadd ps ->
    let nbone = ref 0 in
    let doit ps p =
      let p1 = add p one in
      if Sp.mem p1 ps then (incr nbone; Sp.remove p1 ps)
      else Sp.add p ps in
    let p = mk_add (Sp.elements (List.fold_left doit Sp.empty ps)) in
    if !nbone mod 2 = 0 then p
    else add p one
  | _ -> p

let add p1 p2 =
  if !with_clear then clear_add (add p1 p2)
  else add p1 p2

(* ----------------------------------------------------- *)
(* Multiplication                                        *)

let mul_to_list p =
  match p with
  | PEmul ps -> ps
  | _        -> [p]

let rec merge_mul ps1 ps2 =
  match ps1, ps2 with
  | [], _ -> ps2
  | _, [] -> ps1
  | p1::ps1', p2::ps2' ->
    match compare_p p1 p2 with
    | 0            -> p1 :: merge_mul ps1' ps2'
    | c when c < 0 -> p1 :: merge_mul ps1' ps2
    | _            -> p2 :: merge_mul ps1  ps2'

let mk_mul ps =
  match ps with
  | []  -> one
  | [p] -> p
  | _   -> PEmul ps

let mul p1 p2 =
  if is_zero p1 || is_zero p2 then zero
  else if is_one p1 then p2
  else if is_one p2 then p1
  else
    let ps1 = mul_to_list p1 in
    let ps2 = mul_to_list p2 in
    mk_mul (merge_mul ps1 ps2)

(* Simplify p * !p into 0 *)
let clear_mul p =
  match p with
  | PEmul ps ->
    let doit ps p =
      let p1 = add p one in
      if Sp.mem p1 ps then raise Not_found
      else Sp.add p1 ps in
    begin try
      ignore (List.fold_left doit Sp.empty ps); p
    with Not_found -> zero
    end
  | _ -> p

let mul p1 p2 =
  if !with_clear then clear_mul (mul p1 p2)
  else mul p1 p2

(* ----------------------------------------------------- *)
(* Substitution                                          *)

let rec subst x xp p =
  match p with
  | PEvar x' ->
    if E.equal x x' then xp else p
  | PEconst _ -> p
  | PEadd ps ->
    fold_subst add x xp ps
  | PEmul ps ->
    fold_subst mul x xp ps

and fold_subst op x xp ps =
  match ps with
  | [] -> assert false
  | [p] -> subst x xp p
  | p::ps -> op (subst x xp p) (fold_subst op x xp ps)

(* ------------------------------------------------------------------ *)
(* Generalized gauzz                                                  *)

exception Found of expr

let find_rnd excl ps =

  let rec push_add ms accu =
    match ms with
    | [] -> accu
    | p :: ms ->
      match p with
      | PEadd _ -> push_add ms (p::accu)
      | _ -> push_add ms accu in

  let in_mul x ps =
    let rec aux p =
      match p with
      | PEvar x' -> E.equal x x'
      | PEconst _ -> false
      | PEmul ps | PEadd ps -> List.exists aux ps in
    let doit p =
      match p with
      | PEvar _  -> false
      | PEconst _ -> false
      | PEmul ps -> List.exists aux ps
      | PEadd _  -> assert false in
    List.exists doit ps in

  let rec aux accu ps =
    match ps with
    | [] -> if accu == [] then raise Not_found else aux [] accu
    | PEvar _ :: ps -> aux accu ps
    | PEconst _ :: ps -> aux accu ps
    | PEmul ms :: ps -> aux (push_add ms accu) ps
    | (PEadd ms as p) :: ps ->
      let accu = ref accu in
      let doit p1 =
        match p1 with
        | PEvar x ->
          if is_rnd x && not (Se.mem x excl) && not (in_mul x ms) then
            raise (Found x)
        | PEconst _ -> () 
        | PEmul ms -> accu := push_add ms !accu
        | PEadd _  -> assert false in
      try List.iter doit ms; raise Not_found
      with Found x   -> (x,p)
         | Not_found -> aux !accu ps in

  aux [] ps

exception Indep
exception Depend

let initial_check_indep k ps =
  let tbl = Hv.create 101 in
  let reset () = Hv.clear tbl in
  let get p =
    try Hv.find tbl p
    with Not_found -> (Sint.empty, 0) in
  let add p i =
    let (si, n) = get p in
    if Sint.mem i si then ()
    else
      let n = n + 1 in
      if k < n then raise Depend
      else
        Hv.replace tbl p (Sint.add i si, n) in
  let rec is_indep1 p =
    match p with
    | PEvar { e_node = Eshare(p,i,_) } -> add p i
    | PEvar _ -> ()
    | PEconst _ -> ()
    | PEadd ps | PEmul ps -> List.iter is_indep1 ps in
  let is_indep ps =
    reset ();
    try List.iter is_indep1 ps; true
    with Depend -> false in

  let bij = ref [] in
  let rec reduce excl ps =
    if is_indep ps then (!bij, tbl)
    else
      match find_rnd excl ps with
      | (x,p) ->
        bij := (x,p) :: !bij;
        reduce (Se.add x excl) (List.map (subst x p) ps)
      | exception Not_found -> raise Depend in

  reduce Se.empty ps

(* -------------------------------------------------------------------- *)

let rec find_ty e = 
  match e.e_node with
  | Etop -> assert false
  | Econst c -> c.c_ty
  | Ernd v | Eshare (_,_,v) | Epub v -> v.v_ty
  | Eop1(op, _) | Eop2(op,_,_) ->
    begin match op.op_ty with
    | Some(_,ty) -> ty
    | _          -> assert false
    end
  | Eop(_,op,es) ->
    begin match op.op_ty with
    | Some(_,ty) -> ty
    | None when is_op_tuple op -> find_ty es.(0)
    | _ -> assert false
    end


let to_pol ty tbl =
  let o_add = o_add ty in
  let o_mul = o_mul ty in
  let o_neg = o_neg ty in
  let eone = eone ty in
  let ezero = ezero ty in

  let rec to_pol e =
    try He.find tbl e
    with Not_found ->
      let p =
        match e.e_node with
        | Etop          -> assert false
        | Ernd _ | Eshare _ | Epub _ -> PEvar e
        | Econst _ -> 
          if E.equal eone e then one
          else if E.equal ezero e then zero
          else PEvar e
        | Eop1(o, e) when Op.equal o o_neg ->
          add one (to_pol e) 
        | Eop2(o,e1,e2) when Op.equal o o_add ->
          add (to_pol e1) (to_pol e2)
        | Eop2(o,e1,e2) when Op.equal o o_mul ->
          mul (to_pol e1) (to_pol e2)
        | Eop(_, o, es) when is_FF_op o -> to_pol es.(1)
        | _ -> assert false (* FIXME error msg *)
      in
      He.add tbl e p;
      p in
  to_pol

let rec split_tuple s e =
  match e.e_node with
  | Eop(_, o, es) when is_op_tuple o ->
    Array.fold_left split_tuple s es
  | _ -> e :: s

let to_pol_bij tbl bij ty =

  let apply_bij p =
    List.fold_left (fun p (x,xp) -> subst x xp p) p bij in

  let o_add = o_add ty in
  let o_mul = o_mul ty in
  let o_neg = o_neg ty in
  let eone = eone ty in
  let ezero = ezero ty in

  let rec to_pol_bij e =
    try He.find tbl e
    with Not_found ->
      let p =
        match e.e_node with
        | Etop          -> assert false
        | Ernd _ -> apply_bij (PEvar e)
        | Eshare _ | Epub _ -> PEvar e
        | Econst _ -> 
          if E.equal eone e then one
          else if E.equal ezero e then zero
          else PEvar e
        | Eop1(o, e) when Op.equal o o_neg ->
          add one (to_pol_bij e) 
        | Eop2(o,e1,e2) when Op.equal o o_add ->
          add (to_pol_bij e1) (to_pol_bij e2)
        | Eop2(o,e1,e2) when Op.equal o o_mul ->
          mul (to_pol_bij e1) (to_pol_bij e2)
        | Eop(_, o, es) when is_FF_op o -> to_pol_bij es.(1)
        | _ -> assert false (* FIXME error msg *)
      in
      He.add tbl e p;
      p in
  to_pol_bij

let check_indep k (es:expr list) (other: expr list list) =
(*  Format.eprintf "try gauzz on@.@[<v>%a@]@."
     (pp_list "@ " pp_expr) es; *)

  let ty = 
    match es with
    | [] -> assert false
    | e::_ -> find_ty e in

  (* form expr to pexpr *)

  
  let pol_tbl = He.create 101 in
  let to_pol = to_pol ty pol_tbl in

  let ps =
    let to_pols s e =
      let es = split_tuple [] e in
      List.fold_left (fun s e -> to_pol e :: s) s es in
    List.fold_left to_pols [] es in

  (*Format.eprintf "ps = @[(%a)@]@."
    (pp_list ",@ " pp_p_expr) ps; *)


  let bij, param_tbl = initial_check_indep k ps in

  (* At this point we known that "bij" allows to prove independence of
     es.
     We add all expression of es into the table in_tbl
   *)
  let in_tbl = He.create 101 in
  List.iter (fun e -> He.add in_tbl e ()) es;

  (* We add in in_tbl expression of other that do not depend of the
     secrets after application of the bijection *)

  let ok_tbl = He.create 101 in

  He.clear pol_tbl;
  let to_pol_bij = to_pol_bij pol_tbl bij ty in

  let rec check_depend p =
    match p with
    | PEvar { e_node = Eshare(p,i,_) } ->
      (try
         let (si, _) = Hv.find param_tbl p in
         Sint.mem i si
       with Not_found -> false)
    | PEvar _ -> true
    | PEconst _ -> true
    | PEadd ps | PEmul ps -> List.for_all check_depend ps in

  let check_bij e =
    try He.find ok_tbl e
    with Not_found ->
      let p = to_pol_bij e in
      let b = check_depend p in
      He.replace ok_tbl e b;
      b in

(*  let all = ref 0 in
  let extra = ref 0 in *)

  let doit e =
    if not (He.mem in_tbl e) then
      let es = split_tuple [] e in
(*      incr all; *)
      if List.for_all check_bij es then
        ((*incr extra; *) He.replace in_tbl e ()) in

  List.iter (fun es -> List.iter doit es) other;
(*  Format.eprintf "nb_other = %i; extra = %i@." !all !extra; *)
  in_tbl

let check_indep k (es:expr list) (other: expr list list) =
  with_clear := false;
  try check_indep k es other
  with Depend ->
    with_clear := true;
    check_indep k es other
