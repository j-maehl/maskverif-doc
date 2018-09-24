open Util

let rec lex cmp l1 l2 = 
  match l1, l2 with
  | [], [] -> 0
  | _, []  -> 1
  | [], _  -> -1
  | x1::l1, x2::l2 ->
    match cmp x1 x2 with
    | 0 -> lex cmp l1 l2
    | x -> x

module type VAR = sig
  type t 
  val equal   : t -> t -> bool
  val compare : t -> t -> int
end

exception NotDivisible 
exception DivByZero
exception NotDepend

module Poly(V:VAR) = struct
  type var = V.t
  
  module M (* : sig 
    type t 
    val compare : t -> t -> int 
    val one : t 
    val mul : t -> t -> t
    val of_var : var -> t
    val of_vars : var list -> t
     
    (* [div a b = q] if a = q * b *)
    val div : t -> t -> t 

  end *) = struct 

    type t = var list 

    let equal m1 m2 = List.equal V.equal m1 m2 
     

    let compare = lex V.compare 
 
    let one = [] 

    let rec mul m1 m2 = 
      match m1, m2 with
      | [], _ -> m2
      | _, [] -> m1
      | x1::m1', x2::m2' ->
        match V.compare x1 x2 with
        | 0 -> x1 :: mul m1' m2'
        | c when c < 0 -> x2:: mul m1 m2'
        | _ -> x1:: mul m1' m2 

    let of_var x = [x]

    let of_vars = List.sort_uniq (fun x1 x2 -> -V.compare x1 x2)

    let rec div a b = 
      match a, b with
      | _, [] -> a
      | [], _ -> raise NotDivisible
      | x1::a', x2::b' ->
        match V.compare x1 x2 with
        | 0 -> div a' b'
        | c when c < 0 -> raise NotDivisible
        | _ -> x1::div a' b

    let divo a b =
      try Some (div a b)
      with NotDivisible -> None

    let rec divx a x = 
      match a with
      | [] -> raise NotDivisible
      | x1::a ->
        if V.compare x x1 = 0 then a
        else x1:: divx a x 

    let dependx x m = 
      List.exists (fun x' -> V.compare x x' = 0) m 

    let eval rho m = List.for_all rho m
  end

  type mon = M.t

  type t = mon list

  let equal p1 p2 = List.equal M.equal p1 p2 

  let zero = []
  let one = [M.one]

  let var x = [M.of_var x]

  let rec add p1 p2 = 
    match p1, p2 with
    | [], _ -> p2
    | _, [] -> p1
    | m1::p1', m2::p2' ->
      match M.compare m1 m2 with
      | 0 -> add p1' p2' 
      | c when c < 0 -> m2 :: add p1 p2'
      | _ -> m1 :: add p1' p2 

  let rec insert m1 r = 
    match r with
    | [] -> [m1]
    | m2::r' ->
      match M.compare m1 m2 with
      | 0 -> r'
      | c when c < 0 -> m1 :: r
      | _ -> m2 :: insert m1 r'

  let mulm m p =
    if m = M.one then p 
    else 
      let rec aux r p = 
        match p with
        | [] -> List.rev r 
        | m1::p -> aux (insert (M.mul m m1) r) p in
      aux zero p

  let mul p1 p2 = 
    let rec aux r p1 =
      match p1 with
      | [] -> r
      | m1::p1 -> aux (add r (mulm m1 p2)) p1
    in
    aux zero p1

   (* [check_div A B = Q] if A = Q * B + 0 *)
   let div a b =
     let db = 
       match b with
       | [] -> raise DivByZero
       | db :: _ -> db in
     let rec aux q a =
       match a with
       | [] -> q 
       | da ::_ -> 
         let m = M.div da db in
         aux (add [m] q) (add a (mulm m b)) in
     aux zero a 

   let div_eucl a b =
     let db = 
       match b with
       | [] -> raise DivByZero
       | db :: _ -> db in
     let rec aux q a = 
       match a with
       | [] -> q, zero
       | da :: _ ->
         match M.divo da db with
         | Some m ->
           aux (add [m] q) (add a (mulm m b))
         | None ->
           q, a in
     aux zero a
           
   let divx a x =
     let q = ref [] in
     let r = ref [] in
     let rec aux = function
       | [] -> ()
       | m::a ->
         (try q := M.divx m x :: !q
          with NotDivisible -> r := m :: !r);
         aux a in
     aux a;
     !q, !r

   (* P = (r + P1) * P2 
      P = rP2 + P1P2 
      P = rQ + R
      R = Q1Q
      then 
      P1 = Q1 and P2 = Q *)

   (* [check_rnd p r] return (p1,p2) such that p = (r + p1) * p2
      r should occur in p *)
   let check_rnd p r = 
     try 
       let p2, r = divx p r in
       let p1 = div r p2 in
       Some (p1, p2)
     with NotDivisible -> None

   (* [check_rnd_eucl r p] return (p1,p2,p3) such that p = (r + p1) * p2 + p3.
      r will not occur in p1 p2 p3.
      r should occur in p *)
   let check_rnd_eucl r p = 
       let p2, pr = divx p r in (* p = p2 * r + pr *)
       (* div_eucl pr p2 = (p1, p3) ->
          pr = p1 * p2 + p3  ->
          p = r * p2 + p1 * p2 + p3 ->
          p = (r + p1) * p2 + p3 *)
       let p1,p3 = div_eucl pr p2 in
       (p1, p2, p3)
     
   let dependx x p = 
     List.exists (M.dependx x) p

   let iter_vars f p =
     List.iter (List.iter f) p

   let all_vars f p =
     List.for_all (List.for_all f) p

   let exists_vars f p = 
     List.exists (List.exists f) p

   let fold_vars f p = 
     List.fold_left (List.fold_left f) p

   let eval rho p = 
     List.fold_left (fun r m ->
         let rm = M.eval rho m in
           (r && not rm) || (not r && r)) false p

   let deg p = 
     List.fold_left (fun d m -> max d (List.length m)) 0 p


   let rec inter p1 p2 = 
     match p1, p2 with
     | [], _ -> []
     | _, [] -> []
     | m1::p1', m2::p2' ->
       match M.compare m1 m2 with
       | 0 -> m1 :: inter p1' p2' 
       | c when c < 0 -> inter p1 p2'
       | _ -> inter p1' p2 

   let rec diff p1 p2 = 
     match p1, p2 with
     | [], _ -> []
     | _, [] -> p1
     | m1::p1', m2::p2' ->
       match M.compare m1 m2 with
       | 0 -> diff p1' p2' 
       | c when c < 0 -> m2 :: diff p1 p2'
       | _ -> m1 :: diff p1' p2 

end





