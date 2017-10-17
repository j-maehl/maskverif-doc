require import Byte.

module R = {
 proc refresh (a1 a2 a3 a4 a5 a6 a7:byte) = {
   var r1, r2, r3, r4, r5, r6, r7: byte;

   r1 = $distr; r2 = $distr; r3 = $distr; r4 = $distr; 
   r5 = $distr; r6 = $distr; r7 = $distr;

   a1 = a1 + r1;
   a2 = a2 + r2;
   a3 = a3 + r3;
   a4 = a4 + r4;
   a5 = a5 + r5;
   a6 = a6 + r6;
   a7 = a7 + r7;

   a1 = a1 + r7;
   a2 = a2 + r1;
   a3 = a3 + r2;
   a4 = a4 + r3;
   a5 = a5 + r4;
   a6 = a6 + r5;
   a7 = a7 + r6;

   r1 = $distr; r2 = $distr; r3 = $distr; r4 = $distr; 
   r5 = $distr; r6 = $distr; r7 = $distr;

   a1 = a1 + r1;
   a2 = a2 + r2;
   a3 = a3 + r3;
   a4 = a4 + r4;
   a5 = a5 + r5;
   a6 = a6 + r6;
   a7 = a7 + r7;

   a1 = a1 + r7;
   a2 = a2 + r1;
   a3 = a3 + r2;
   a4 = a4 + r3;
   a5 = a5 + r4;
   a6 = a6 + r5;
   a7 = a7 + r6;

   return (a1,a2,a3,a4,a5,a6,a7);
 }
}.

masking sni 6 R.refresh Byte.ComRing.(+).

