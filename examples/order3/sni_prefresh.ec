require import Byte.

module R = {
 proc refresh (a1 a2 a3 a4:byte) = {
   var r1, r2, r3, r4: byte;

   r1 = $distr; r2 = $distr; r3 = $distr; r4 = $distr;

   a1 = a1 + r1;
   a2 = a2 + r2;
   a3 = a3 + r3;
   a4 = a4 + r4;

   a1 = a1 + r4;
   a2 = a2 + r1;
   a3 = a3 + r2;
   a4 = a4 + r3;

   return (a1,a2,a3,a4);
 }
}.

masking sni 3 R.refresh Byte.ComRing.(+).

