open Util
open Expr 
open State

module C = Shrcnt

type ldfs = (int * (expr * expr) list * int) list

let lfirst ldfs = 
  let rec first n hd tl = 
    if n = 0 then hd 
    else match tl with
    | x::tl -> first (n-1) (x::hd) tl
    | []   -> assert false in
  let rec aux hd ldfs = 
    match ldfs with
    | [] -> hd
    | (d,fs,_)::ldfs -> aux (first d hd fs) ldfs in
  aux [] ldfs

let set_top_ldfs state (ldfs:ldfs) = 
  List.iter (fun (_, es, _) -> 
      List.iter (fun (e,_) -> ignore (add_top_expr state e)) es) ldfs

let simplify_ldfs state maxparams ldfs = 
  clear_state state;
  set_top_ldfs state ldfs;
  init_todo state;
  let res = simplify_until state maxparams in
  if res then (* Not need to do more work just return *)
    false, ldfs
  else 
    let simple = simplified_expr state in
    true, List.map (fun (d, es, len) -> 
               (d, List.map (fun (e1, e2) -> simple e1, e2) es, len)) ldfs
    

let set_top_exprs state es = 
  List.iter (fun (e,_) -> ignore (add_top_expr state e)) es

let set_top_exprs2 state ldfs = 
  List.iter (fun (_,es,_) -> set_top_exprs state es) ldfs

exception CanNotCheck of (expr * expr) list 
  
let print_error fmt lhd = 
  Format.fprintf fmt "@[<v>Cannot check@ %a@ reduce to@ %a@]"
    (pp_list ",@ " (fun fmt (_,e2) -> pp_expr fmt e2)) lhd
    (pp_list ",@ " (fun fmt (e1,_) -> pp_expr fmt e1)) lhd

let find_bij state maxparams ldfs =
  let lhd = lfirst ldfs in
  clear_state state;
  set_top_exprs state lhd;
  init_todo state;
  if not (simplify_until state maxparams) then 
    let simple = simplified_expr state in
    let lhd = List.map (fun (e1,e2) -> simple e1, e2) lhd in
    raise (CanNotCheck lhd)
  else
    let used_share = used_share state in
    set_top_exprs2 state ldfs;
    clear_bijection state;    
    simplify_until_with_clear state used_share maxparams;
    let is_in (e1, _) = is_top_expr state e1 in
    List.map (fun (_,es,_) -> List.partition is_in es) ldfs


let cnp n p =
  let rec aux n p m d =
    if n = 1 then Z.div (Z.mul (Z.of_int p) m) d
    else aux (n-1) (p-1) (Z.mul (Z.of_int p) m) (Z.mul (Z.of_int n) d) in
  if n = 0 then Z.one 
  else aux n p Z.one Z.one

let cnp_accu accu =
  List.fold_left (fun r (n,_,p) -> Z.mul r (cnp n p)) Z.one accu
 
let pp_z fmt z = 
  let s = Z.to_string z in
  let len = Bytes.length s in
  let s' = 
    let m = len mod 3 in
    if m = 0 then ""
    else Bytes.make (3 - m) '0' in

  let s = s' ^ s in
  let k = Bytes.length s / 3 in
  for i = 0 to k - 1 do
    for j = 0 to 2 do
      Format.fprintf fmt "%c" s.[i*3 + j]
    done;
    if i <> k-1 then Format.fprintf fmt "," 
  done

let pp_human suffix fmt num =
  let sfx = [| ""; "K"; "M"; "G"; "T"; "P"; "E"; "Z"; "Y" |] in
  let idx = ref 0 in
  let num = ref num in
  let fcr = ref None in

  while !idx < (Array.length sfx - 1) && Z.gt !num (Z.of_int 1000) do
    fcr := Some (Z.rem !num (Z.of_int 1000));
    num := Z.div !num (Z.of_int 1000);
    idx := !idx + 1
  done;
  let suffix = sfx.(!idx) ^ suffix in

  match !fcr with
  | None ->
      Format.fprintf fmt "%s %s" (Z.to_string !num) suffix
  | Some fcr ->
      Format.fprintf fmt "%s.%0.3d %s"
        (Z.to_string !num) (Z.to_int fcr) suffix
      
  
let check_all state maxparams (ldfs:ldfs) = 
  let to_check = cnp_accu ldfs in
  Format.eprintf "%a tuples to check@." pp_z to_check;
  let tdone = ref Z.zero in
  let count = ref 0 in
  let t0 = Sys.time () in
  let rec check_all state maxparams (ldfs:ldfs) = 
    incr count;    
    if !count land 0x3FFFF = 0 then 
      Format.eprintf "%a tuples checked over %a in %.3f@."
        pp_z !tdone pp_z to_check (Sys.time () -. t0);

    let continue, ldfs = simplify_ldfs state maxparams ldfs in
    if continue then 
      let split = find_bij state maxparams ldfs in
      let rec aux accu split ldfs = 
        match split, ldfs with
        | [], [] -> tdone := Z.add !tdone (cnp_accu accu)
        | (s1, s2) :: split, (d,_,len) :: ldfs ->
          let len1 = List.length s1 in
          let len2 = len - len1 in
          aux ((d,s1,len1)::accu) split ldfs;
          let ldfs = List.rev_append accu ldfs in
          if d <= len2 then 
            check_all state maxparams ((d,s2,len2)::ldfs);
          for i1 = 1 to d - 1 do
            let i2 = d - i1 in
            if i1 <= len1 && i2 <= len2 then
              check_all state maxparams ((i1,s1,len1)::(i2,s2,len2)::ldfs)
          done
        | _ -> assert false in
      aux [] split ldfs 
    else 
      tdone := Z.add !tdone (cnp_accu ldfs);
   in
  check_all state maxparams (ldfs:ldfs);
  Format.eprintf "%a tuples checked@." pp_z !tdone

exception Done

let check_all_para state maxparams (ldfs:ldfs) = 
  let pipe   = Unix.pipe () in
  let tdone  = Shrcnt.create "/masking.para.tdone" in
  let tprcs  = Shrcnt.create "/masking.para.tprcs" in
  let t0     = Sys.time () in
  let parent = ref true in
  let pp     = pp_human "tuples" in

  let cleanup () =
    if !parent then begin
      (try Shrcnt.dispose tdone with _ -> ());
      (try Shrcnt.dispose tprcs with _ -> ())
    end
  in

  try
    let to_check = cnp_accu ldfs in

    Format.eprintf "%a to check@." pp to_check;

    if Unix.fork () = 0 then begin
      let count = ref 0 in

      Shrcnt.clr_unlink_on_dispose tdone;
      Shrcnt.clr_unlink_on_dispose tprcs;
      parent := false; Unix.close (fst pipe);
      Shrcnt.update tprcs 1L;

      if Unix.fork () <> 0 then exit 0;

      let rec check_all state maxparams (ldfs:ldfs) = 
        let goup, stend = ref false, ref false in

          if (incr count; !count) > 256 then begin
            count := 0; if Shrcnt.get tprcs < 16L then begin
            let pid = Unix.fork () in
            if pid = 0 then begin
              if Unix.fork () = 0 then begin
                stend := true
              end else begin
               Shrcnt.update tprcs 1L; exit 0
              end
            end else begin
              goup := true;
              ignore (Unix.waitpid [] pid : int * _)
            end
          end
        end;

        if not !goup then begin
          let continue, ldfs = simplify_ldfs state maxparams ldfs in
  
          if continue then 
            let split = find_bij state maxparams ldfs in
            let rec aux accu split ldfs = 
              match split, ldfs with
              | [], [] ->
                  Shrcnt.update tdone (Z.to_int64 (cnp_accu accu))
  
              | (s1, s2) :: split, (d,_,len) :: ldfs ->
                  let len1 = List.length s1 in
                  let len2 = len - len1 in
                  aux ((d,s1,len1)::accu) split ldfs;
                  let ldfs = List.rev_append accu ldfs in
                  if d <= len2 then 
                    check_all state maxparams ((d,s2,len2)::ldfs);
                  for i1 = 1 to d - 1 do
                    let i2 = d - i1 in
                    if i1 <= len1 && i2 <= len2 then
                      check_all state maxparams ((i1,s1,len1)::(i2,s2,len2)::ldfs)
                  done
  
              | _ -> assert false
  
            in aux [] split ldfs
          else 
            Shrcnt.update tdone (Z.to_int64 (cnp_accu ldfs))
        end;

        if !stend then raise Done in

       try
         check_all state maxparams (ldfs:ldfs); raise Done
       with
       | CanNotCheck le -> begin
           Format.eprintf "%a@." print_error le;
           Shrcnt.update tprcs (-1L);
           ignore (Unix.write (snd pipe) "." 0 1 : int);
           exit 1
         end

       | Done -> begin
           Shrcnt.update tprcs (-1L);
           exit 0
         end
      
    end else begin
      Unix.close (snd pipe);
      let rec wait () =
        let rds, _, _ = Unix.select [fst pipe] [] [] 5.0 in
        if List.is_empty rds then begin
          Format.eprintf
            "%a checked over %a in %.3f@."
            pp (Z.of_int64 (Shrcnt.get tdone))
            pp to_check (Sys.time () -. t0);
          wait ()
        end else begin
          Unix.read (fst pipe) (Bytes.make 1 '.') 0 1 = 0
        end
      in

      if not (wait ()) then exit 1;
      Format.eprintf "%a checked@." pp (Z.of_int64 (Shrcnt.get tdone));
      if Shrcnt.get tprcs <> 0L then assert false
    end; cleanup ()

  with e -> (cleanup (); raise e)

let check_ni params nb_shares interns outs =
  try 
    let all = interns @ outs in
    let len = List.length all in
    let state = init_state nb_shares params in
    let order = nb_shares - 1 in 
    let all = List.map (fun e -> e, e) all in
    let args = [order, all, len] in
    check_all state order args;
    Format.printf "NI@."
  with CanNotCheck le ->
    Format.eprintf "%a@." print_error le

let check_fni ?(para = false) s f params nb_shares interns outs =
  try 
    let len_i = List.length interns in
    let len_o = List.length outs in
    let state = init_state nb_shares params in
    let order = nb_shares - 1 in 
    let interns = List.map (fun e -> e, e) interns in
    let outs = List.map (fun e -> e, e) outs in 
    for ki = 0 to (*nb_shares - 1*) (nb_shares-1)/2 (* FIXME *) do
      let ko = order - ki in
      Format.eprintf "Start checking of ki = %i, ko = %i@." ki ko;
      (if ki <= len_i && ko <= len_o then
         let args = if ko = 0 then [] else [ko, outs, len_o] in
         let args = if ki = 0 then args else (ki,interns,len_i) :: args in
         (if para then check_all_para else check_all) state (f ki ko) args);
      Format.eprintf "Checking of done ki = %i, ko = %i@." ki ko;
    done;
    Format.printf "%s@." s
  with CanNotCheck le ->
    Format.eprintf "%a@." print_error le

let check_sni ?para = check_fni ?para "SNI" (fun ki _ko -> ki)
let check_fni ?para = check_fni ?para "FNI" 

let mk_interns outs = 
  let souts = Se.of_list outs in
  let rec aux interns e = 
    if not (Se.mem e interns) then
      let interns = 
        if not (Se.mem e souts) then Se.add e interns else interns in
      match e.e_node with
      | Eadd(e1,e2) | Emul(e1,e2) -> aux (aux interns e1) e2
      | _ -> interns 
    else interns in
  let interns = List.fold_left aux Se.empty outs in
  Se.elements interns

let main_sni params nb_shares outs = 
  check_sni ~para:true params nb_shares (mk_interns outs) outs

let main_ni params nb_shares outs = 
  check_ni params nb_shares (mk_interns outs) outs

let main_fni f params nb_shares outs = 
  check_fni f params nb_shares (mk_interns outs) outs

(* ------------------------------------------------------------------ *)


let mk_rnd s = rnd (V.mk_var s)
let ( ++ ) x1 x2 = add x1 x2
let ( ** ) x1 x2 = mul x1 x2 

let a = V.mk_var "a" 
let b = V.mk_var "b" 

let vrand s n = 
  Array.init n (fun i -> mk_rnd (s^string_of_int i))

let vadd v1 v2 = 
  let n = Array.length v1 in
  assert (n = Array.length v2);
  Array.init n (fun i -> v1.(i) ++ v2.(i))

let vmul v1 v2 = 
  let n = Array.length v1 in
  assert (n = Array.length v2);
  Array.init n (fun i -> v1.(i) ** v2.(i))

let vshift v k = 
  let n = Array.length v in
   Array.init n (fun i -> v.((i + n - k) mod n)) 

let vshare a n = Array.init n (share a) 

let refresh_a a n r k l = 
  let r = vrand r n in
  let rk = vshift r k in
  let c = vadd r rk in
  let rec aux c l = 
    match l with
    | (r,k) :: l ->
      let r = vrand r n in
      let rk = vshift r k in
      let c = vadd (vadd c r) rk in
      aux c l
    | [] -> 
      vadd c a in
  aux c l
  
let refresh n r k l = 
  refresh_a (vshare a n) n r k l

let refresh2 n k = refresh n "r" 1 ["s", k] 

let doit1 n k = 
  let refresh = refresh n "r" k [] in
  Array.iter (fun e -> Format.printf "%a@." pp_expr e) refresh;
  main_sni [a] n (List.tl (Array.to_list refresh))

let doit1_fni n k = 
  let refresh = refresh n "r" k [] in
  Array.iter (fun e -> Format.printf "%a@." pp_expr e) refresh;
  main_fni
    (fun k1 k2 -> if k1 > 0 && k2 > 1 then k1 + k2 - 2 else k1)
    [a] n (List.tl (Array.to_list refresh))

let doit2 n k = 
  let refresh = refresh2 n k in
  Array.iter (fun e -> Format.printf "%a@." pp_expr e) refresh;
  main_sni [a] n (List.tl (Array.to_list refresh))

let doit3 n k1 k2  = 
  let refresh = refresh n "r" 1 ["s", k1; "t", k2] in
  Array.iter (fun e -> Format.printf "%a@." pp_expr e) refresh;
  main_sni [a] n (List.tl (Array.to_list refresh))

(* Ok in 0.002 s *)
(* let _ = doit1 3 1 *)

(* Ok in 0.002 s *)
(*let _ = doit1 4 1 *)

(* Ok in 0.003 s *)
(*let _ = doit1 5 1 *)

(* Marche pas *)
(* let _ = doit1 6 1 *)

(* Marche pas *)
(* let _ = doit1 6 2 *)

(* Ok in 0.005 s *)
(* let _ = doit2 6 2 *)

(* Ok in 0.022 s *)
(* let _ = doit2 7 2 *)

(* Ok in 0m0.039 s *)
(* let _ = doit2 8 1 *)

(* Ok in 0m0.494 s *)
let _ = doit2 9 1

(* Marche pas *)
(* let _ = doit2 10 1 *)

(* Ok in 0m2.781 s *) 
(* let _ = doit3 10 1 1 *)

(* Ok in 0.059 s *)
(* let _ = doit2 8 2 *)

(* Ok in 0.48 s *)
(* let _ = doit2 9 2 *)

(* Ok in 1.755 s *)
(* let _ = doit2 10 2 *)

(* Ok in 18.231 s *)
(* let _ = doit2 11 2 *)

(* Ok in 49.044 s *)
(* let _ = doit2 12 2 *)

(* Ok in 10m31.869 s *)
(*let _ = doit2 13 2 *)

(* 
  decalage de 2   
  6 internal -> donc permet 6 output -> 13 shares
  decalage de 3 
  8 internal -> donc permet 8 output -> 17 shares
  decalage de 4 
  10 internal -> donc permet 10 output -> 21 shares
  decalage de 5 
  12 internal -> donc permet 12 output -> 25 shares
  decalage de 6 -> 
  14 internal -> donc permet 14 output -> 29 shares
  4n+ 5 
*)

(* Marche pas *)
(* let _ = doit2 14 2 *)
(* 
$s7,
$r9 + $r8 + $s9 + $s7 + a9,
$r11 + $r10 + $s11 + $s9 + a11,
$r13 + $r12 + $s13 + $s11 + a13,
$r1 + $r0 + $s1 + $s13 + a1,
$r1 + $r0 + $s1,

$s6,
$r8 + $r7 + $s8 + $s6 + a8,
$r10 + $r9 + $s10 + $s8 + a10,
$r12 + $r11 + $s12 + $s10 + a12,
$s12,

$r7,
$r13
*)
(* Ok in 31m34.685s *)
(*let _ = doit2 14 3*)

(*let _ = doit1_fni 5 1 *)

(* Ok *)
(* let _ = doit2 15 3 *)

let mul8 = 
  let n = 8 in
  let a = vshare a n   in
  let b = vshare b n   in

  let c = vmul a b   in

  let r = vrand "r" n in

  let c = vadd c r in
  let c = vadd (vadd c (vmul a (vshift b 1))) (vmul (vshift a 1) b) in

  let c = vadd c (vshift r 1) in
  let c = vadd (vadd c (vmul a (vshift b 2))) (vmul (vshift a 2) b) in

  let r = vrand "s" n in
  let c = vadd c r in
  let c = vadd (vadd c (vmul a (vshift b 3))) (vmul (vshift a 3) b) in

  let c = vadd c (vshift r 1) in
  let c = vadd c (vmul a (vshift b 4)) in

  c

(* ok 101 minutes *)
(*
let _ = 
  Array.iter (fun e -> Format.printf "%a@." pp_expr e) mul8;
  main_ni [a;b] 8 (List.tl (Array.to_list mul8))
*)

(* marche pas *)
(*
let _ = 
  Array.iter (fun e -> Format.printf "%a@." pp_expr e) mul8;
  main_sni [a;b] 8 (List.tl (Array.to_list mul8))
*)

(* marche pas 
let mul8_sni = 
  let t = mk_rnd "t" in
  let vt = Array.make 8 t in
  vadd mul8 vt

let _ = 
  Array.iter (fun e -> Format.printf "%a@." pp_expr e) mul8_bis;
  main_sni [a;b] 8 (List.tl (Array.to_list mul8_bis))

 *)

let mul9 = 
  let n = 9 in
  let a = vshare a n   in
  let b = vshare b n   in

  let c = vmul a b   in

  let r = vrand "r" n in

  let c = vadd c r in
  let c = vadd (vadd c (vmul a (vshift b 1))) (vmul (vshift a 1) b) in

  let c = vadd c (vshift r 1) in
  let c = vadd (vadd c (vmul a (vshift b 2))) (vmul (vshift a 2) b) in

  let r = vrand "s" n in
  let c = vadd c r in
  let c = vadd (vadd c (vmul a (vshift b 3))) (vmul (vshift a 3) b) in

  let c = vadd c (vshift r 1) in
  let c = vadd (vadd c (vmul a (vshift b 4))) (vmul (vshift a 4) b) in

  c

(* ok 
let _ = 
  Array.iter (fun e -> Format.printf "%a@." pp_expr e) mul9;
  main_ni [a;b] 9 (List.tl (Array.to_list mul9))
 *)

let mk_mul10 a b = 
  let n = 10 in
  
  let c = vmul a b   in

  let r = vrand "r" n in

  let c = vadd c r in
  let c = vadd (vadd c (vmul a (vshift b 1))) (vmul (vshift a 1) b) in

  let c = vadd c (vshift r 1) in
  let c = vadd (vadd c (vmul a (vshift b 2))) (vmul (vshift a 2) b) in

  let r = vrand "s" n in
  let c = vadd c r in
  let c = vadd (vadd c (vmul a (vshift b 3))) (vmul (vshift a 3) b) in

  let c = vadd c (vshift r 1) in
  let c = vadd (vadd c (vmul a (vshift b 4))) (vmul (vshift a 4) b) in

  let r = vrand "t" n in
  let c = vadd c r in
  let c = vadd c (vmul a (vshift b 5)) in
  let c = vadd c (vshift r 1) in 
  c

let mul10 = 
  let n = 10 in
  let a = vshare a n   in
  let b = vshare b n   in
  mk_mul10 a b 

let square10 = 
  let n = 10 in
  let a = vshare a n   in
  let a1 = refresh_a a n "u" 1 [] in
  let a2 = refresh_a a n "v" 2 [] in
  mk_mul10 a1 a2
  

(*
let _ = 
  Array.iter (fun e -> Format.printf "%a@." pp_expr e) square10;
  main_ni [a;b] 10 (List.tl (Array.to_list square10)) 
 *)

(*
let () =
  let cnt = Shrcnt.create "/nu/strub/counter" in

  for i = 0 to 1 do
    if Unix.fork () = 0 then
      while true do
        Printf.printf "%ld\n%!" (Shrcnt.update cnt 1l)
      done
  done;

  while true do Unix.pause () done
 *)
