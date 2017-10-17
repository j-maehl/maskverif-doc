let parse () = 
  let n = int_of_string (Sys.argv.(1)) in (* Masking order *)
  let k = Array.length Sys.argv in 
  let dec = Array.sub Sys.argv 2 (k-2) in
  let dec = Array.map int_of_string dec in
  n, dec

(* Les share a0....an *)
let rnds = [|"r";"s";"t";"u";"v";"w";"x";"y";"z"|]

let print fmt n dec = 
  
  let pp_var fmt (name,i) = 
    Format.fprintf fmt "%s%i" name i in

  let pp_vars fmt name =
    for i = 0 to n-1 do 
      Format.fprintf fmt "%a, " pp_var (name, i)
    done;
    pp_var fmt (name, n) in

  let pp_rnd fmt r = 
    for i = 0 to n do
      Format.fprintf fmt "%a <$ distr;@ " pp_var (r,i)
    done;
    Format.fprintf fmt "@ "
  in
  
  let pp_sum fmt r1 r2 k = 
    for i = 0 to n do
      let j = if k <= i then i - k else i - k + (n + 1) in 
      Format.fprintf fmt "%a = %a + %a;@ " 
        pp_var ("a",i) pp_var (r1,i) pp_var (r2,j) 
    done;
    Format.fprintf fmt "@ "
  in

  let pp_sums fmt i k = 
    let r = rnds.(i) in
    pp_rnd fmt r;
    if i = 0 then pp_sum fmt r r k
    else begin
      pp_sum fmt "a" r 0;
      pp_sum fmt "a" r k
    end;
    Format.fprintf fmt "@ "
  in
      
  Format.fprintf fmt "@[<v>require import Byte.@ @ ";
  Format.fprintf fmt "module M = {@ ";
  Format.fprintf fmt "  @[<v>proc refresh(%a) = {@ " pp_vars "b";
  Format.fprintf fmt "  @[<v>";
  Format.fprintf fmt "var %a;@ " pp_vars "a";
  Array.iteri (fun i _ -> 
      Format.fprintf fmt "var %a;@ " pp_vars rnds.(i)) dec;
  Array.iteri (pp_sums fmt) dec;
  pp_sum fmt "a" "b" 0;
  Format.fprintf fmt "return (%a);@]@ }@]@ }.@ @ " pp_vars "a";
  Format.fprintf fmt "masking sni %i M.refresh Byte.ComRing.(+).@ @]" n

let _ = 
  let n, dec = parse () in
  print Format.std_formatter n dec


(*  

 a0 + r0 + rn + s0] + sn-1 
 a1 + r1 + r0 + s1] + sn
 a2 + r2 + r1 + s2] + s0]
 a3 + r3 + r2 + s3  + s1]
 a4 + r4 + r3 + s4 + s2
 a5 + r5] + r4 + s5] + s3]
 ...
 an + rn] + rn-1 + sn + sn-2


 a0 + r0 + rn + s0 + sn-1 
 a1 + r1 + r0 + s1 + s
 a2 + r2 + r1 + s2 + s0
 a3 + r3 + r2 + s3 + s1
 a4 + r4 + r3 + s4 + s2
 a5 + r5 + r4 + s5 + s3
 a6 + r6 + r5 + s6 + s4
 a7 + r7 + r6 + s7 + s5
 ...
 an + rn + rn-1 + sn + sn-2


3 output
7 input



 a0 + r0 + rn + s0] + sn-1 
 a1 + r1 + r0] + s1 + sn
 a2 + r2 + r1 + s2 + s0]
 a3 + r3 + r2] + s3 + s1
 a4 + r4] + r3 + s4] + s2]
 ...
 an + rn] + rn-1 + sn + sn-2

2 outputs
6 inputs
t1 + t2 - 2


si t2 <= 2 alors t1
sinon t1 + t2 - 3

 a0 + r0 + rn + s0] + sn-1 
 a1 + r1 + r0] + s1 + sn
 a2 + r2 + r1 + s2] + s0]
 ...
 an + rn] + rn-1 + sn + sn-2


 a0 + r0 + rn + s0] + sn-1 
 a1 + r1 + r0 + s1] + sn
 a2 + r2 + r1 + s2] + s0]
 a3 + r3] + r2 + s3] + s1]
 ...
 an + rn] + rn-1 + sn + sn-2


 
 a0 + r0 + rn + s0] + sn-1 
 a1 + r1 + r0 + s1] + sn
 a2 + r2 + r1 + s2 + s0]
 a3 + r3 + r2 + s3 + s1]
 a4 + r4 + r3 + s4] + s2]
 a5 + r5] + r4 + s5] + s3]
 ...
 an + rn] + rn-1 + sn + sn-2

 4 output
 6 internal

7  t1+t2 -3





3 obs 
6
1   10 obs ok


0 +
1    -
2 +
3    - 
4 +
5    -
6 +
7


0  +
1         -
2     -
3  +     
4     -
5         -
6  +  
7         -
8     -
9  +
10
11


 *)
