require import Byte.

module R = {
 proc refresh (a1 a2 a3 a4 a5:byte) = {
   var r1, r2, r3, r4, r5: byte;

   r1 = $distr; r2 = $distr; r3 = $distr; r4 = $distr; r5 = $distr;

   a1 = a1 + r1;
   a2 = a2 + r2;
   a3 = a3 + r3;
   a4 = a4 + r4;
   a5 = a5 + r5;

   a1 = a1 + r5;
   a2 = a2 + r1;
   a3 = a3 + r2;
   a4 = a4 + r3;
   a5 = a5 + r4;

   r1 = $distr; r2 = $distr; r3 = $distr; r4 = $distr; r5 = $distr;

   a1 = a1 + r1;
   a2 = a2 + r2;
   a3 = a3 + r3;
   a4 = a4 + r4;
   a5 = a5 + r5;

   a1 = a1 + r5;
   a2 = a2 + r1;
   a3 = a3 + r2;
   a4 = a4 + r3;
   a5 = a5 + r4;

   return (a1,a2,a3,a4,a5);
 }
}.

masking sni 4 R.refresh Byte.ComRing.(+).

