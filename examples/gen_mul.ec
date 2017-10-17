require import Int IntDiv Byte.

require import Array.

op d : int.

op shares = d + 1.


op array_init : (int -> 'a) -> int -> 'a array.

op array_create : 'a -> int -> 'a array.

module M = {
  proc mul(a b:byte array) = {
    var c : byte array;
    var i,j : int;
    var ab, ri, ri1, rd, r : byte;
    
    c = array_create oner shares;

    i=0; while(i <= d) {
      c.[i] = a.[i] * b.[i];
      i = i + 1;
    }

    j=0; while(j < d%/4) {

      rd = $distr;
      ri1 = rd;
      i = 0;
      while(i <= d) {
        r = $distr;
        ri = if (i=d) then rd else r;
        c.[i] = c.[i] + ri;
        ab = a.[i] * b.[(i-j-1) %% shares];
        c.[i] = c.[i] + ab;
        ab = a.[(i-j-1) %% shares] * b.[i];
        c.[i] = c.[i] + ab;
        c.[i] = c.[i] + ri1;
        ab = a.[i] * b.[(i-j-2) %% shares];
        c.[i] = c.[i] + ab;
        ab = a.[(i-j-2) %% shares] * b.[i];
        c.[i] = c.[i] + ab;
        i = i + 1;
      }

      j = j + 1;
    }

   if (d%/4 = 1) {
     (* Pas de refresh en plus *)
     rd = $distr;
     ri1 = rd;
     i = 0;
     while(i <= d) {
       r = $distr;
       ri = if (i=d) then rd else r;
       c.[i] = c.[i] + ri;
       ab = a.[i] * b.[(i-j-1) %% shares];
       c.[i] = c.[i] + ab;
       c.[i] = c.[i] + ri1;
       i = i + 1;
     }
   }

   if (d%/4 = 2) {

     rd = $distr;
     ri1 = rd;
     i = 0;
     while(i <= d) {
       r = $distr;
       ri = if (i=d) then rd else r;
       c.[i] = c.[i] + ri;
       ab = a.[i] * b.[(i-j-1) %% shares];
       c.[i] = c.[i] + ab;
       ab = a.[(i-j-1) %% shares] * b.[i];
       c.[i] = c.[i] + ab;
       c.[i] = c.[i] + ri1;
       i = i + 1;
     }
   }

   if (d%/4 = 3) {
     rd = $distr;
     ri1 = rd;
     i = 0;
     while(i <= d) {
       r = $distr;
       ri = if (i=d) then rd else r;
       c.[i] = c.[i] + ri;
       ab = a.[i] * b.[(i-j-1) %% shares];
       c.[i] = c.[i] + ab;
       ab = a.[(i-j-1) %% shares] * b.[i];
       c.[i] = c.[i] + ab;
       c.[i] = c.[i] + ri1;
       ab = a.[i] * b.[(i-j-2) %% shares];
       c.[i] = c.[i] + ab;
       i = i + 1;
     }
   }

   if (d%/4 <> 1) { (* Je sais pas s'il le faut aux ordre plus grands mais pour d = 5 pas necessaire *) 
     (* On rajoute un demi refresh *)
     rd = $distr;
     ri1 = rd;
     i = 0;
     while(i <= d) {
       r = $distr;
       ri = if (i=d) then rd else r;
       c.[i] = c.[i] + ri;
       c.[i] = c.[i] + ri1;
       i = i + 1;
     }
   } 
 }
}.

        
        
      
      
      
      

      i=0; while(i <= d) {
        
        c.[i] = c.[i] + ri;
        ab =  a.[i] * b.[(i - j - 1) %% shares];
        c.[i] = c[i] + ab;
        ab = a.[(i - j - 1) %% shares] * b.[i];
        i = i + 1;
        
    }

      ab = a[i
      

      
     i = i + 1;
     
    }  
  }
}.



