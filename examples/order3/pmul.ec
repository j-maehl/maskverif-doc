require import Byte.

module M = {
  proc pmul (a1 a2 a3 a4 b1 b2 b3 b4) = {
   var c1, c2, c3, c4, r1, r2, r3, r4:byte;
   var ij:byte;

   r1 = $distr; r2 = $distr; r3 = $distr; r4 = $distr;

   c1 = a1 * b1;
   c2 = a2 * b2;
   c3 = a3 * b3;
   c4 = a4 * b4;

   c1 = c1 + r1;
   c2 = c2 + r2;
   c3 = c3 + r3;
   c4 = c4 + r4;

   ij = a1 * b4;
   c1 = c1 + ij;
   ij = a2 * b1;
   c2 = c2 + ij;
   ij = a3 * b2;
   c3 = c3 + ij;
   ij = a4 * b3;
   c4 = c4 + ij;

   ij = a4 * b1;
   c1 = c1 + ij;
   ij = a1 * b2;
   c2 = c2 + ij;
   ij = a2 * b3;
   c3 = c3 + ij;
   ij = a3 * b4;
   c4 = c4 + ij;

   c1 = c1 + r4;
   c2 = c2 + r1;
   c3 = c3 + r2;
   c4 = c4 + r3;
   
   ij = a1 * b3;
   c1 = c1 + ij;
   ij = a2 * b4;
   c2 = c2 + ij;
   ij = a3 * b1;
   c3 = c3 + ij;
   ij = a4 * b2;
   c4 = c4 + ij;
 
   r1 = $distr; r2 = $distr; r3 = $distr; r4 = $distr;
   c1 = c1 + r1;
   c2 = c2 + r2;
   c3 = c3 + r3;
   c4 = c4 + r4;
   c1 = c1 + r4;
   c2 = c2 + r1;
   c3 = c3 + r2;
   c4 = c4 + r3;

   return (c1,c2,c3,c4);
}
}.

masking sni 3 M.pmul Byte.ComRing.(+).

