(* ----------------------------------------------------------------------- *)
module List = struct
  include List

  (* Count the number of elements in a list satisfying predicate [f] *)
  let count f =
    fold_left (fun n a -> if f a then n+1 else n) 0

  (* Map a function [f] over a list, also returning the size of the list *)
  let map_size f l =
    let size = ref 0 in
    let f x = incr size; f x in
    let l = map f l in
    l, !size

  (* Check if a list is empty *)
  let is_empty l = l == []

  (* Compare two lists for equality using a custom equality function [f] *)
  let rec equal f l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | x1::l1, x2::l2 -> f x1 x2 && equal f l1 l2
    | _, _ -> false

  (* Split a list into two parts: first [n] elements and the rest *)
  let rec split n l =
    if n <= 0 then ([], l)
    else match l with
    | [] -> assert false
    | x::l -> let (l1,l2) = split (n-1) l in (x::l1, l2)
end

(* Partition a list [l] into two lists [lin] and [lout] based on predicate [f] *)
let rec partition f lin lout l =
  match l with
  | [] -> lin, lout
  | e::l ->
    if f e then partition f (e::lin) lout l
    else partition f lin (e::lout) l

(* Create a list of integers from [i] to [j] (inclusive), in order *)
let mk_range_i i j =
  let i1,j1 = if i <= j then i,j else j,i in
  let l = ref [] in
  for k = j1 downto i1 do
    l := k :: !l
  done;
  if i <= j then !l else List.rev !l

(* ----------------------------------------------------------------------- *)
type 'a pp = Format.formatter -> 'a -> unit
(* Pretty-printer type: takes a formatter and a value *)

(* Pretty-print a list with a separator [sep] and element printer [pp] *)
let rec pp_list sep pp fmt xs =
  let pp_list = pp_list sep pp in
  match xs with
  | []      -> ()
  | [x]     -> Format.fprintf fmt "%a" pp x
  | x :: xs -> Format.fprintf fmt "%a%(%)%a" pp x sep pp_list xs

(* Enclose pretty-printed output with [pre] and [post] strings *)
let pp_enclose ~pre ~post pp fmt x =
  Format.fprintf fmt "%(%)%a%(%)" pre pp x post

(* Pretty-print with parentheses *)
let pp_paren pp fmt x =
  pp_enclose ~pre:"(" ~post:")" pp fmt x

(* Conditional pretty-print: use [pp1] if [c] is true, else [pp2] *)
let pp_if c pp1 pp2 fmt x =
  match c with
  | true  -> Format.fprintf fmt "%a" pp1 x
  | false -> Format.fprintf fmt "%a" pp2 x

(* Maybe apply a transformation [tx] to [pp] if [c] is true *)
let pp_maybe c tx pp fmt x =
  pp_if c (tx pp) pp fmt x

(* Maybe parenthesize pretty-printing if [c] is true *)
let pp_maybe_paren c pp =
  pp_maybe c pp_paren pp

(* ----------------------------------------------------------------------- *)
module OrderedInt = struct
  type t = int
  let compare x y = x - y
end

module Sint = Set.Make(OrderedInt)
module Mint = Map.Make(OrderedInt)

(* ----------------------------------------------------------------------- *)
module OrderedStr = struct
  type t = string
  let compare x y = compare x y
end

module Mstr = Map.Make(OrderedStr)

(* ----------------------------------------------------------------------- *)
module Array = struct
  include Array

  (* Check if all elements of array [t] satisfy predicate [f] *)
  let for_all f t =
    let rec aux i = f t.(i) && (i = 0 || aux (i-1)) in
    aux (length t - 1)

  (* Check if all corresponding elements of arrays [t1] and [t2] satisfy [f] *)
  let for_all2 f t1 t2 =
    let n1 = length t1 in
    n1 = length t2 &&
      (n1 = 0 ||
         let rec aux i = f t1.(i) t2.(i) && (i = 0 || aux (i-1)) in
         aux (n1 - 1))
end

(* Ensure [final] is always called after [f a], even if an exception occurs *)
let finally final f a =
  let res = try f a with e -> final();raise e in
  final(); res

(* ----------------------------------------------------------------------- *)
exception EmptyStack

(* Simple stack implementation with dynamic resizing *)
module Stack = struct
  type 'a t = {
    mutable st_buff : 'a array;
    mutable st_top  : int
  }

  (* Copy the stack *)
  let copy t = {
      st_top = t.st_top;
      st_buff = Array.sub t.st_buff 0 t.st_top;
    }

  (* Create a stack with initial size [n] and default value [a] *)
  let make n a = {
    st_buff = Array.make n a;
    st_top  = 0;
  }

  (* Push an element onto the stack, resizing if needed *)
  let push s a =
    let top = s.st_top in
    let len = Array.length s.st_buff in
    if top = len then begin
      let nlen = 2*len + 1 in
      let nbuff = Array.make nlen a in
      Array.blit s.st_buff 0 nbuff 0 len;
      s.st_buff <- nbuff
    end;
    if not (0 <= top && top < Array.length s.st_buff) then
      Format.eprintf "top = %i; len = %i@." top (Array.length s.st_buff);
    s.st_buff.(top) <- a;
    s.st_top <- top + 1

  (* Pop an element from the stack *)
  let pop s =
    let top = s.st_top in
    if top = 0 then raise EmptyStack;
    let top = top - 1 in
    s.st_top <- top;
    s.st_buff.(top)

  (* Clear the stack *)
  let clear s = s.st_top <- 0

  (* Convert the stack to a list *)
  let to_list s = Array.to_list (Array.sub s.st_buff 0 s.st_top)

  (* Iterate over the stack *)
  let iter f s =
    for i = 0 to s.st_top - 1 do f s.st_buff.(i) done

  (* Map a function over the stack *)
  let map f s =
    { st_top = s.st_top;
      st_buff = Array.init s.st_top (fun i -> f s.st_buff.(i));
    }
end

(* -------------------------------------------------------------------- *)
(* Simple vector implementation with dynamic resizing *)
module Vector = struct

  type 'a t = {
    mutable last : int;
    mutable arr  : 'a array;
  }

  (* Copy the vector *)
  let copy t = {
      last = t.last;
      arr = Array.sub t.arr 0 t.last;
    }

  (* Get the number of elements in the vector *)
  let size v = v.last

  (* Create a vector with initial size and default value *)
  let create size default =
    { last = 0;
      arr  = Array.make size default; }

  (* Resize the vector if needed *)
  let resize v =
    let len = Array.length v.arr in
    if v.last = len then begin
      if len = 0 then assert false;
      let narr = Array.make (2 * len) v.arr.(0) in
      Array.blit v.arr 1 narr 1 (len - 1);
      v.arr <- narr
    end

  (* Get the element at index [n] *)
  let get v n =
    if 0 <= n && n < v.last then Array.unsafe_get v.arr n
    else raise (Invalid_argument "index out of bounds")

  (* Remove the element at index [n] *)
  let unset v n =
    if 0 <= n && n < v.last then begin
      if n <> v.last - 1 then
        Array.blit v.arr (n+1) v.arr n (v.last - (n+1));
      v.last <- v.last - 1
    end else raise (Invalid_argument "index out of bounds")

  (* Push an element onto the vector *)
  let push v a =
    resize v;
    let n = v.last in
    v.last <- n + 1;
    v.arr.(n) <- a

  (* Remove all elements satisfying [test] *)
  let remove test v =
    let i = ref 0 in
    let j = ref 0 in
    let size = v.last in
    let arr = v.arr in
    while (!i < size) do
      if test arr.(!i) then incr i
      else
        begin
          if !i <> !j then arr.(!j) <- arr.(!i);
          incr i; incr j
        end
    done;
    v.last <- !j

  (* Iterate over the vector *)
  let iter f v =
    for i = 0 to v.last - 1 do f v.arr.(i) done

  (* Pop the last element *)
  let pop v =
    let r = v.arr.(v.last - 1) in
    v.last <- v.last - 1;
    r

  (* Get the last element *)
  let top v =
    v.arr.(v.last - 1)

  (* Dummy vector (unsafe, for internal use) *)
  let dummy () = Obj.magic {
     arr = Array.make 0 1;
     last = 0;
  }

  (* Convert the vector to a list *)
  let to_list v =
    let l = ref [] in
    for i = v.last - 1 downto 0 do
      l := v.arr.(i) :: !l
    done;
    !l

  (* Clear the vector *)
  let clear v = v.last <- 0

  (* Check if any element satisfies [f] *)
  let exists f v =
    let rec aux i = i < v.last && (f v.arr.(i) || aux (i+1)) in
    aux 0
end

(* ----------------------------------------------------------------- *)

(* Source code location type *)
type location = {
  lc_fname : string;         (* File name *)
  lc_start : int * int;      (* Start line and column *)
  lc_end   : int * int;      (* End line and column *)
  lc_bchar : int;            (* Start character offset *)
  lc_echar : int;            (* End character offset *)
}

(* -------------------------------------------------------------------- *)
(* Value with location information *)
type 'a located = { pl_data: 'a; pl_location: location; }

(* Create a located value *)
let mkloc (lc : location) (x : 'a) =
  { pl_data = x; pl_location = lc; }

let loc  { pl_location = lc } = lc
let data { pl_data     = dt } = dt

(* -------------------------------------------------------------------- *)

(* Location utilities for lexing/parsing *)
module Location : sig
  open Lexing

  type t = location

  val make      : position -> position -> t
  val of_lexbuf : lexbuf -> t
  val to_string : t -> string
end = struct
  open Lexing

  type t = location

  (* Create a location from two lexing positions *)
  let make (p1 : position) (p2 : position) =
    let mkpos (p : position) = (p.pos_lnum, p.pos_cnum - p.pos_bol) in
    { lc_fname = p1.pos_fname;
      lc_start = mkpos p1    ;
      lc_end   = mkpos p2    ;
      lc_bchar = p1.pos_cnum ;
      lc_echar = p2.pos_cnum ; }

  (* Get location from a lexbuf *)
  let of_lexbuf (lexbuf : lexbuf) =
    let p1 = Lexing.lexeme_start_p lexbuf in
    let p2 = Lexing.lexeme_end_p lexbuf in
    make p1 p2

  (* Convert a location to a human-readable string *)
  let to_string (lc : t) =
    let spos =
      if lc.lc_start = lc.lc_end then
        Printf.sprintf "line %d (%d)"
          (fst lc.lc_start) (snd lc.lc_start)
      else if fst lc.lc_start = fst lc.lc_end then
        Printf.sprintf "line %d (%d-%d)"
          (fst lc.lc_start) (snd lc.lc_start) (snd lc.lc_end)
      else
        Printf.sprintf "line %d (%d) to line %d (%d)"
          (fst lc.lc_start) (snd lc.lc_start)
          (fst lc.lc_end  ) (snd lc.lc_end  )
    in
      Printf.sprintf "%s: %s" lc.lc_fname spos
end

(* Print a warning message, optionally with location *)
let warning ?loc fmt =
  let buf  = Buffer.create 127 in
  let bfmt = Format.formatter_of_buffer buf in
  let loc =
    match loc with
    | None -> ""
    | Some loc -> "at "^(Location.to_string loc) in
  Format.kfprintf (fun _ ->
    Format.pp_print_flush bfmt ();
    let msg = Buffer.contents buf in
    Format.eprintf "Warning %s: %s@." loc msg) bfmt fmt

exception Error of (string * Location.t option * string)

(* Raise an error with a message and optional location *)
let error s loc fmt =
  let buf  = Buffer.create 127 in
  let bfmt = Format.formatter_of_buffer buf in
  Format.kfprintf (fun _ ->
    Format.pp_print_flush bfmt ();
    let msg = Buffer.contents buf in
    raise (Error(s,loc,msg))) bfmt fmt

(* Pretty-print an error *)
let pp_error fmt (s,loc,msg) =
  let pp_loc fmt loc =
    match loc with
    | None -> ()
    | Some loc -> Format.fprintf fmt " at %s" (Location.to_string loc) in
  Format.fprintf fmt "%s%a: %s" s pp_loc loc msg

(* -------------------------------------------------------------------- *)
exception ParseError of Location.t * string option
exception LexicalError of Location.t option * string

(* -------------------------------------------------------------------- *)
(* 
   Hash-consed string type and module.
   This is used to efficiently represent and compare strings (such as identifiers)
   by assigning each unique string a unique integer id.
*)

type hstring = {
    hs_id : int;      (* Unique integer id for the string *)
    hs_str : string;  (* The actual string value *)
  }

module HS = struct
  let id = ref 0  (* Counter for assigning unique ids *)

  let tbl = Hashtbl.create 100  (* Table for string -> hstring mapping *)

  (* Create or retrieve a hash-consed string for [s] *)
  let make s =
    try Hashtbl.find tbl s
    with Not_found ->
      let p = { hs_id = !id; hs_str = s } in
      incr id;
      Hashtbl.add tbl s p;
      p

  type t = hstring  (* Alias for the hash-consed string type *)

  (* Physical equality for fast comparison *)
  let equal s1 s2 = s1 == s2

  (* Hash function based on the unique id *)
  let hash s = s.hs_id

  (* Comparison function for ordering *)
  let compare s1 s2 = s1.hs_id - s2.hs_id

  (* Pretty-printer for hash-consed strings.
     If [full] is true, prints both the string and its id, otherwise just the string. *)
  let pp full fmt s = 
    if full then
      Format.fprintf fmt "%s#%i" s.hs_str s.hs_id
    else  Format.fprintf fmt "%s" s.hs_str
end

(* ----------------------------------------------------------------- *)

(* Pretty-print a big integer [z] with commas every 3 digits *)
let pp_z fmt z =
  let s = Z.to_string z in
  let len = String.length s in
  let s' =
    let m = len mod 3 in
    if m = 0 then ""
    else String.make (3 - m) '0' in

  let s = s' ^ s in
  let k = String.length s / 3 in
  for i = 0 to k - 1 do
    for j = 0 to 2 do
      Format.fprintf fmt "%c" s.[i*3 + j]
    done;
    if i <> k-1 then Format.fprintf fmt ","
  done

(* Pretty-print a big integer [num] in human-readable form with suffixes *)
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

(* ------------------------------------------------------------- *)
let verbosity = ref 0

(* Set the verbosity level *)
let set_verbose i = verbosity := i

(* Get a formatter for the given verbosity level *)
let fverbose i =
  if i <= !verbosity then Format.fprintf else Format.ifprintf

let everbose i = fverbose i Format.err_formatter
let verbose i = fverbose i Format.std_formatter

(* -------------------------------------------------------------- *)

(* Tool options type *)
type tool_opt = {
    pp_error  : bool;   (* Pretty-print errors *)
    checkbool : bool;   (* Enable boolean checking *)
  }
