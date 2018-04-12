open Util
open Expr 
open State
open Checker

(*
let mk_rnd =
  let c = ref 0 in
  fun () -> 
    let r = rnd (V.mk_var ("r" ^ string_of_int !c)) in
    incr c; 
    r 

let ( ++ ) x1 x2 = add x1 x2
let ( ** ) x1 x2 = mul x1 x2 

let a = V.mk_var "a" 
let b = V.mk_var "b" 

let ea = share a 0 
let eb = share b 0 

let share e t = 
   let rs = Array.init t (fun _ -> mk_rnd ()) in
   let mk_share i = 
      if i = t then
        let s = ref e in
        for i = 0 to t-1 do 
          s := !s ++ rs.(i)
        done;
        box !s
      else rs.(i) in
   Array.init (t + 1) mk_share 

let array_map2 f t1 t2 = 
  Array.mapi (fun i a -> f a t2.(i)) t1

let sadd t1 t2 = 
  assert (Array.length t1 = Array.length t2);
  array_map2 add t1 t2  

let smul t1 t2 = 
   assert (Array.length t1 = Array.length t2);
   let t = Array.length t1 - 1 in
   let c = array_map2 mul t1 t2 in
   for i = 0 to t do
     for j = i + 1 to t do
       let r = mk_rnd () in
       c.(i) <- c.(i) ++ r;
       c.(j) <- ((c.(j) ++ r) ++ (t1.(i) ** t2.(j))) ++ (t1.(j) ** t2.(i))
      done
    done;
    c

let _ = 
  let order = 6 in
  let t1 = share ea order in
  let t2 = share eb order in
  let es = smul (sadd t1 t2) t1 in
  Format.printf "(*********************************)@.";
  Format.printf "%a" (pp_list "@." pp_expr) (Array.to_list es);
  Format.printf "@.(*********************************)@.";
  main_threshold order [a; b] es   


 *)
















































(*

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
      vadd c a (*Lazy.force a*) in
  aux c l
  
let refresh n r k l = 
  refresh_a (vshare a n) (*Lazy.from_fun (fun () -> vshare a n)*) n r k l

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
(* let _ = doit2 9 1 *)

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
 let _ = doit2 12 2 

(* Ok in 10m31.869 s *)
(* let _ = doit2 13 2 *)

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

(*
let square10 = 
  let n = 10 in
  let a = vshare a n   in
  let a1 = refresh_a a (*Lazy.from_val a*) n "u" 1 [] in
  let a2 = refresh_a a (*Lazy.from_val a*) n "v" 2 [] in
  mk_mul10 a1 a2
 *)

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

*)
