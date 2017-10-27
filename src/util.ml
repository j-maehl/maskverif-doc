(* ----------------------------------------------------------------------- *)
module List = struct
  include List

  let count f = 
    fold_left (fun n a -> if f a then n+1 else n) 0 

  let map_size f l = 
    let size = ref 0 in
    let f x = incr size; f x in
    let l = map f l in
    l, !size

  let is_empty l = l == []

end

(* ----------------------------------------------------------------------- *)
let rec pp_list sep pp fmt xs =
  let pp_list = pp_list sep pp in
  match xs with
  | []      -> ()
  | [x]     -> Format.fprintf fmt "%a" pp x
  | x :: xs -> Format.fprintf fmt "%a%(%)%a" pp x sep pp_list xs

let rec partition f lin lout l = 
  match l with
  | [] -> lin, lout
  | e::l -> 
    if f e then partition f (e::lin) lout l
    else partition f lin (e::lout) l

(* ----------------------------------------------------------------------- *)
module OrderedInt = struct
  type t = int
  let compare x y = x - y
end

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
  
  let for_all f t = 
    let rec aux i = f t.(i) && (i = 0 || aux (i-1)) in
    aux (length t - 1)
end 

let finally final f a =
  let res = try f a with e -> final();raise e in
  final(); res

(* ----------------------------------------------------------------------- *)
exception EmptyStack

module Stack = struct
  type 'a t = {
    mutable st_buff : 'a array;
    mutable st_top  : int
  }

 let copy t = {
      st_top = t.st_top;
      st_buff = Array.sub t.st_buff 0 t.st_top;
    }

  let make n a = {
    st_buff = Array.make n a;
    st_top  = 0;
  }
    
  let push s a = 
    let top = s.st_top in
    let len = Array.length s.st_buff in
    if top = len then begin
      let nlen = 2*len + 1 in
      let nbuff = Array.make nlen a in
      Array.blit s.st_buff 0 nbuff 0 len 
    end;
    s.st_buff.(top) <- a;
    s.st_top <- top + 1

  let pop s = 
    let top = s.st_top in
    if top = 0 then raise EmptyStack;
    let top = top - 1 in
    s.st_top <- top;
    s.st_buff.(top)

  let clear s = s.st_top <- 0

  let to_list s = Array.to_list (Array.sub s.st_buff 0 s.st_top)

  let iter f s = 
    for i = 0 to s.st_top - 1 do f s.st_buff.(i) done
end 

(* -------------------------------------------------------------------- *)
module Vector = struct

  type 'a t = {
    mutable last : int;
    mutable arr  : 'a array;
  }

  let copy t = {
      last = t.last;
      arr = Array.sub t.arr 0 t.last;
    }

  let size v = v.last

  let create size default = 
    { last = 0;
      arr  = Array.make size default; }

  let resize v = 
    let len = Array.length v.arr in
    if v.last = len then begin
      if len = 0 then assert false;
      let narr = Array.make (2 * len) v.arr.(0) in 
      Array.blit v.arr 1 narr 1 (len - 1);
      v.arr <- narr
    end

  let get v n = 
    if 0 <= n && n < v.last then Array.unsafe_get v.arr n
    else raise (Invalid_argument "index out of bounds")

  let unset v n =
    if 0 <= n && n < v.last then begin
      if n <> v.last - 1 then
        Array.blit v.arr (n+1) v.arr n (v.last - (n+1));
      v.last <- v.last - 1
    end else raise (Invalid_argument "index out of bounds")
    
  let push v a = 
    resize v;
    let n = v.last in
    v.last <- n + 1;
    v.arr.(n) <- a

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

  let iter f v = 
    for i = 0 to v.last - 1 do f v.arr.(i) done

  let pop v = 
    let r = v.arr.(v.last - 1) in
    v.last <- v.last - 1;
    r
 
  let top v = 
    v.arr.(v.last - 1)
    
  let dummy () = Obj.magic {
     arr = Array.make 0 1;
     last = 0;
  }
  
  let to_list v = 
    let l = ref [] in
    for i = v.last - 1 downto 0 do
      l := v.arr.(i) :: !l
    done;
    !l

  let clear v = v.last <- 0

  let exists f v = 
    let rec aux i = i < v.last && (f v.arr.(i) || aux (i+1)) in
    aux 0

end
   


      

    




