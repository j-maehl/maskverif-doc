require import Byte.

module M = {

proc refresh_5_6(a1 a2 a3 a4 a5:byte) = {
  var r1, r2, r3, r4, r5, r6:byte;
  
  r1 = $distr;
  r2 = $distr;
  r3 = $distr;
  r4 = $distr;
  r5 = $distr;
  r6 = $distr;

  r6 = r5;

  a1 = a1 + r1;
  a2 = a2 + r1;
  
  a1 = a1 + r2;
  a3 = a3 + r2;
  
  a4 = a4 + r3;
  a5 = a5 + r3;
  
  a1 = a1 + r4;
  a4 = a4 + r4;
  
  a3 = a3 + r5;
  a5 = a5 + r5;
  
  a2 = a2 + r6;
  a5 = a5 + r6;
  
  return (a1,a2,a3,a4,a5);
}

}.

masking sni 4 M.refresh_5_6 Byte.ComRing.(+).

module M = {
  proc refresh (a1 a2 a3 a4 a5 a6) = {
   var c1, c2, c3, c4, c5, c6, r1, r2, r3, r4, r5, r6, r7, r8:byte;
   var ij:byte;

   r1 = $distr; r2 = $distr; r3 = $distr; r4 = $distr;
   r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;

   c1 = a1 + r1;
   c1 = c1 + a2;
   c2 = r1;

   c3 = a3 + r2;
   c3 = c3 + a4;
   c4 = r2;

   c5 = a5 + r3;
   c5 = c5 + a6;
   c6 = r3;

   c1 = a1 + r1;
   c1 = c1 + a2;
   c2 = r1;
   
  
   c1 = c1 + r4;
   c1 = c1 + a5;
   c5 = r4;

   c1 = c1 + r5;
   c1 = c1 + a6;
   c6 = r5;

   c2 = c2 + r6;
   c4 = c4 + r6;

   c3 = c3 + r7;
   c5 = c5 + r7;

   c6 = c6 + r8;
   c1 = c1 + r8;

   return (c1,c2,c3,c4,c5,c6);
}
}.

masking sni 5 M.refresh Byte.ComRing.(+).
