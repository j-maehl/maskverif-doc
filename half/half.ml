open BatList

let op i j = if i < j then (i,j) else (j,i);;

let range_except (i : int) (j : int) (ps : int list): int list =
  filter (fun i-> not (mem i ps)) (range i `To j);;

let pivot (p : int) (is : int list): (int * int) list =
  List.map (fun i-> op i p) is;;

let rec cross_append (pres : 'a list list) (sufs : 'a list list) acc =
  match pres with
  | []        -> acc
  | (p::pres) -> cross_append pres sufs ((List.map (fun l-> p @ l) sufs) @ acc);;

(*** Generating Permutations ***)
let rec interleave x lst = 
  match lst with
  | [] -> [[x]]
  | hd::tl -> (x::lst) :: (List.map (fun y -> hd::y) (interleave x tl));;

let rec gen_perms lst = 
  match lst with
  | hd::tl -> List.concat (List.map (interleave hd) (gen_perms tl))
  | _ -> [lst];;

let perm_range i j ps = gen_perms (range_except i j ps);;

(*** Generating additive refresh prefix ***)
let rec prefix p (is : int list) (acc : (int * int) list): (int * int) list =
  match is with
  | []    -> acc
  | i::is -> prefix p is ((op i p)::acc);;

(*** Printing a refresh list out for input into other tools ***)
let print_int_pair (i,j) = Printf.sprintf "%d %d" i j;;
let print_int_pairs l = String.concat "  " (List.map print_int_pair l);;
let print_int_pairs_list l = String.concat "\n" (List.map print_int_pairs l);;

(*** Take a list of pivots and get to work ***)
let generate (n : int) (ps : int list) =
  let rec aux_gen (n : int) (ps : int list) = function
    | []   -> [[]]
    | p::l -> begin
	let fin = aux_gen n (p::ps) l in
	let ls  = List.map (pivot p) (perm_range 1 n (p::ps)) in
	cross_append ls fin []
	end
  in
  match ps with
  | [] -> failwith "Why are you asking me this? Have you no shame?"
  | p::l -> (let ls = aux_gen n [p] l in
             map (prefix p (range_except 1 n [p])) ls);;

let () =
  let args = Array.to_list (Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1)) in
  let args = map int_of_string args in
  let ls =
    begin
    match args with
    | []    -> failwith "Expecting at least one integer argument."
    | [n]   -> generate n [1;n]
    | n::ps -> generate n ps
    end
  in Printf.printf "%s\n" (print_int_pairs_list ls)
