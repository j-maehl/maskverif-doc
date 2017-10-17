open Graph
open Graphviz

open DotAttributes

type var = | Share of int | Rand of int | Out
let print_var = function
  | Share i -> "a" ^ (string_of_int i)
  | Rand  i -> "r" ^ (string_of_int i)
  | Out     -> "O"

let shape: var -> Graph.Graphviz.DotAttributes.vertex = function
  | Out     -> `Shape `Doublecircle
  | Share _ -> `Shape `Box
  | Rand  _ -> `Shape `Circle

type edge = | Internal of var * var list | OutVars of var list | OutExprs of var list list

let print_edge = function
  | Internal (v,rs) -> String.concat " + " (List.map print_var (v::rs))
  | OutVars   es    -> failwith "Should not appear at end of computation."
  | OutExprs  es    ->
      begin
      let ls = List.map (List.map print_var) es in
      let ss = List.map (String.concat " + ") ls in
      String.concat "\n" ss
      end;;

module G = struct
  module Var = struct
    type t = var
    let compare = Pervasives.compare
    let equal = (=)
    let hash = Hashtbl.hash
  end
  module Int = struct
    type t = edge
    let compare = Pervasives.compare
    let equal = (=)
    let hash = Hashtbl.hash
    let default = OutVars []
  end

  include Imperative.Digraph.ConcreteLabeled(Var)(Int)
end

module Display = struct
  include G
  let vertex_name v = print_var v
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes v = [shape v]
  let default_edge_attributes _ = []
  let edge_attributes e = [`Label (print_edge (G.E.label e)); `Fontsize 6]
  let get_subgraph _ = None
end

module P = Dot(Display)

let add_share g i =
  G.add_vertex g (Share i)

let add_internal_rand g k =
  if   (G.mem_vertex g (Rand k))
  then failwith "anomaly";
  G.add_vertex g (Rand k)

let add_output_rand g k i j =
  let out_expr =
    (if   not (G.mem_vertex g i)
     then [i] else [])
    @ (if   not (G.mem_vertex g j)
        then [j] else []) in
  G.add_vertex g (Rand k);
  G.add_edge_e g (G.E.create (Rand k) (OutVars out_expr) Out)

let add_edge g v1 v2 =
  if   G.mem_edge g v1 v2
  then failwith "There is already an edge here.\n"
  else
    match v1 with
    | Share _ -> G.add_edge_e g (G.E.create v1 (Internal (v1,[])) v2)
    | _       -> G.add_edge g v1 v2;;

let update_label (g : G.t) (i : var) (k : var) (k' : var): unit =
  let l = G.E.label (G.find_edge g i k) in
  G.remove_edge g i k;
  match l with
  | Internal (v,rs) -> G.add_edge_e g (G.E.create i (Internal (v,k'::rs)) k)
  | _ -> failwith "Cannot update an output edge.";;

let update_labels (g : G.t) (k : var) (i : var): unit =
  G.iter_succ_e (fun e-> update_label g (G.E.src e) (G.E.dst e) k) g i

let add (g : G.t) (k : int) ((i,j) : int * int) =
  if   G.mem_vertex g (Share i)
    && G.mem_vertex g (Share j)
  then add_internal_rand g k
  else add_output_rand   g k (Share i) (Share j);
  add_share g i;
  add_share g j;
  update_labels g (Rand k) (Share i);
  update_labels g (Rand k) (Share j);
  add_edge g (Share i) (Rand k);
  add_edge g (Share j) (Rand k) ;;

let compute_outlabel g r i =
  let l = G.E.label (G.find_edge g i r) in
  match l with
  | Internal (v,rs) -> (v :: (rs @ [r]))
  | _ -> failwith "Not good."

let update_outlabels g r =
  let l = G.E.label (G.find_edge g r Out) in
  let vs =
    begin
    match l with
    | OutVars vs -> vs
    | _ -> failwith "Not good either."
    end in
  let es = List.map (compute_outlabel g r) vs in
  G.remove_edge g r Out;
  G.add_edge_e g (G.E.create r (OutExprs es) Out);;

let finalize g =
  G.iter_pred (update_outlabels g) g Out;;

let construct_graph (g : G.t) (l : (int * int) list) =
  G.add_vertex g Out;
  List.iteri (fun i-> add g (List.length l - i)) (List.rev l);
  finalize g;;

let parse_args args =
  if (Array.length args) mod 2 = 1
  then failwith "Expecting an even number of arguments.";
  let l = ref [] in
  for i = 0 to (Array.length args) / 2 - 1 do
    l := (int_of_string (Array.get args (2 * i)), int_of_string (Array.get args (2 * i + 1))) :: !l;
  done;
  rev !l;;

let main argv =
  let l = parse_args (Array.sub argv 1 (Array.length argv - 1)) in
  let g = G.create () in
  construct_graph g l;
  P.output_graph stdout g;;

let () = main Sys.argv;;
