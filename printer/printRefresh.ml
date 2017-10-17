let range a b =
  let rec int_range_rec l a b =
    if a > b then l
    else int_range_rec (b :: l) a (b - 1)
  in (int_range_rec [] a b);;

(*** Generating strings from Variables and Other Things ***)
let gen_var (v : string) (i : int): string = v ^ (string_of_int i);;

let gen_smp (): string = "r  <$ distr;\n";;

let gen_ass (v : string) (i : int): string =
  (gen_var v i) ^ " <- " ^ (gen_var v i) ^ " + r;\n";;

let gen_instp v (p : int * int): string list =
  let (i1,i2) = p in [gen_smp (); gen_ass v i1; gen_ass v i2];;

let gen_body v (l : (int * int) list): string list =
  List.fold_left (fun is p-> is @ (gen_instp v p)) [] l;;

let gen_args v n: string list =
  List.map (gen_var v) (range 1 n);;

let tabs n =
  String.concat "" (List.map (fun _-> "  ") (range 1 n));;

let print_perm l =
  "["
  ^ String.concat ";"
      (List.map (fun (i,j)-> "(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")") l)
  ^ "]";;

let print_comment d l =
  (tabs d) ^ "(* " ^ (print_perm l) ^ " *)\n";;

let print_proc_prologue p d v n =
  (tabs d) ^ "proc " ^ p ^ "(" ^ (String.concat ", " (gen_args v n)) ^ ") = {\n";;

let print_var d =
  (tabs d) ^ "var r : byte;\n\n";;

let print_body d ss =
  (tabs d) ^ String.concat (tabs d) ss;;

let print_proc_epilogue p d v n =
    (tabs (d + 1)) ^ "return (" ^ (String.concat ", " (gen_args v n)) ^ ");\n"
  ^ (tabs d) ^ "}\n";;

let print_proc p d v n (l : (int * int) list): string =
    (print_comment d l)
  ^ (print_proc_prologue p d v n)
  ^ (print_var (d + 1))
  ^ (print_body (d + 1) (gen_body v l))
  ^ (print_proc_epilogue p d v n)

(*** Top-Level ***)
let print_masking m p s l =
  print_comment 0 l
  ^ "masking sni " ^ (string_of_int (s - 1))
  ^ " " ^ m ^ "." ^ p ^ " Byte.ComRing.(+).";;

let process m p v s l =
  let proc = print_proc p 1 v s l in
  Printf.printf
    "require import Byte.\n\nmodule %s = {\n%s}.\n\n%s\n"
    m proc (print_masking m p s l);;

(*** CLI ***)
let rec max l acc =
  match l with
  | []       -> acc
  | (_,j)::l -> if acc < j then max l j else max l acc;;

let parse_args args =
  if (Array.length args) mod 2 = 1
  then failwith "Expecting an even number of arguments.";
  let l = ref [] in
  for i = 0 to (Array.length args) / 2 - 1 do
    l := (int_of_string (Array.get args (2 * i)), int_of_string (Array.get args (2 * i + 1))) :: !l;
  done;
  List.rev !l;;

let main argv =
  let l = parse_args (Array.sub argv 1 (Array.length argv - 1)) in
  process "R" "refresh" "a" (max l 1) l;;

let () = main Sys.argv;;
