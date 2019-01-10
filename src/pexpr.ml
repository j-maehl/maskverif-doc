open Util
open Expr


type p_expr =
  | PEvar of expr  
  | PEadd of p_expr list 
  | PEmul of p_expr list 

let zero = PEvar efalse
let one  = PEvar etrue

let is_zero p = 
  match p with
  | PEvar x -> E.equal x efalse
  | _ -> false

let is_one p =
  match p with
  | PEvar x -> E.equal x efalse
  | _ -> false 

let rec compare_p p1 p2 = 
  match p1, p2 with
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

(* ----------------------------------------------------- *)
(* Substitution                                          *)

let rec subst x xp p = 
  match p with
  | PEvar x' -> 
    if E.equal x x' then xp else p
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
      | PEmul ps | PEadd ps -> List.exists aux ps in
    let doit p = 
      match p with
      | PEvar _  -> false 
      | PEmul ps -> List.exists aux ps
      | PEadd _  -> assert false in
    List.exists doit ps in

  let rec aux accu ps = 
    match ps with
    | [] -> if accu == [] then raise Not_found else aux [] accu
    | PEvar _ :: ps -> aux accu ps
    | PEmul ms :: ps -> aux (push_add ms accu) ps
    | (PEadd ms as p) :: ps ->
      let accu = ref accu in
      let doit p1 = 
        match p1 with
        | PEvar x ->
          if is_rnd x && not (Se.mem x excl) && not (in_mul x ms) then
            raise (Found x)
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

let to_pol tbl = 
  let rec to_pol e = 
    try He.find tbl e 
    with Not_found ->
      let p = 
        match e.e_node with
        | Etop          -> assert false
        | Ernd _ | Eshare _ | Epub _ -> PEvar e 
        | Eneg e        -> add one (to_pol e)
        | Eadd(e1,e2)   -> add (to_pol e1) (to_pol e2)
        | Emul(e1,e2)   -> mul (to_pol e1) (to_pol e2)
        | Eop(_, o, es) -> 
          if is_FF_op o then to_pol es.(1)
          else if E.equal e etrue then one 
          else if E.equal e efalse then zero
          else assert false in
      He.add tbl e p;
      p in
  to_pol 

let rec split_tuple s e = 
  match e.e_node with
  | Eop(_, o, es) when is_op_tuple o ->
    Array.fold_left split_tuple s es 
  | _ -> e :: s 

let to_pol_bij tbl bij = 

  let apply_bij p = 
    List.fold_left (fun p (x,xp) -> subst x xp p) p bij in
    
  let rec to_pol_bij e = 
    try He.find tbl e 
    with Not_found ->
      let p = 
        match e.e_node with
        | Etop          -> assert false
        | Ernd _        -> apply_bij (PEvar e)
        | Eshare _ | Epub _ -> PEvar e 
        | Eneg e        -> add one (to_pol_bij e)
        | Eadd(e1,e2)   -> add (to_pol_bij e1) (to_pol_bij e2)
        | Emul(e1,e2)   -> mul (to_pol_bij e1) (to_pol_bij e2)
        | Eop(_, o, es) -> 
          if is_FF_op o then to_pol_bij es.(1)
          else if E.equal e etrue then one 
          else if E.equal e efalse then zero
          else assert false in
      He.add tbl e p;
      p in
  to_pol_bij

let check_indep k (es:expr list) (other: expr list list) = 
(*  Format.eprintf "try gauzz on@.@[<v>%a@]@."
     (pp_list "@ " pp_expr) es; *)

  (* form expr to pexpr *)

  let pol_tbl = He.create 101 in
  let to_pol = to_pol pol_tbl in

  let ps = 
    let to_pols s e = 
      let es = split_tuple [] e in
      List.fold_left (fun s e -> to_pol e :: s) s es in
    List.fold_left to_pols [] es in

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
  let to_pol_bij = to_pol_bij pol_tbl bij in

  let rec check_depend p = 
    match p with
    | PEvar { e_node = Eshare(p,i,_) } -> 
      (try 
         let (si, _) = Hv.find param_tbl p in
         Sint.mem i si
       with Not_found -> false)
    | PEvar _ -> true
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
