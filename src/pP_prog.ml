open Util
open State 

let pp_param fmt i = Format.fprintf fmt "a%i" (i+1)

let pp_args sep fmt nb_shares = 
  let params = Array.to_list (Array.init nb_shares (fun i -> i)) in
  pp_list sep pp_param fmt params
            
let pp_prog fmt nb_shares nb_rnds prog = 
  Format.fprintf fmt "@[<v>proc refresh_%i_%i(%a:byte) = {@ " 
    nb_shares nb_rnds (pp_args " ") nb_shares;
  Format.fprintf fmt "  @[<v>var r:byte;@ ";
  let pp_block (i,j) = 
    Format.fprintf fmt "@ @[<v>r = $distr;@ ";
    Format.fprintf fmt "%a = %a + r;@ " pp_param i pp_param i;
    Format.fprintf fmt "%a = %a + r;@]@ " pp_param j pp_param j in
  List.iter pp_block prog;
  Format.fprintf fmt "@ return (%a);@]@ " (pp_args ",") nb_shares;
  Format.fprintf fmt "}@]@."

let rec mk_sni i =
  match i with
  | 2 -> [(1,2)]
  | 3 -> [(1,2); (1,3); (2,3)]
  | 4 -> [(1,2); (3,4); (1,3); (2,4)]
  | 5 -> [(1,2); (1,3); (4,5); (1,4); (3,5); (2,5)]
  | 6 -> [(1,2); (1,3); (1,4); (1,5); (1,6); (3,6); (3,5); (2,3); (3,4)]
  | 7 -> [(1,2); (1,3); (1,4); (1,5); (1,6); (1,7); (2,7); (2,3); (2,6); (4,6); (5,7)]
  | 8 -> [(1,2);(1,3);(1,4);(1,5);(1,6);(1,7);(1,8);
          (3,8); (6,8);(4,8);(5,8);(7,8);(2,8)]
  | 9 -> [(1,2); (1,3); (1,4); (1,5); (1,6); (1,7); (8,9); 
          (1,8); (3,8); (6,8); (7,8); (2,8); (4,7); (7,9); (5,7)]
  | _ ->
    let k = i/2 in
    let pre = Array.to_list (Array.init k (fun i -> i+1, i+1+k))  in
    let sni1 = mk_sni k in
    let sni2 = List.map (fun (i,j) -> i+k,j+k) (mk_sni (i - k)) in
    List.flatten [pre; sni1; sni2; pre]

let pp_li fmt = 
  List.iter (fun (i,j) -> Format.fprintf fmt "%i %i  " i j)

let pp_best fmt i =
  Format.fprintf fmt "%a@." pp_li (mk_sni i)
  
    


  



