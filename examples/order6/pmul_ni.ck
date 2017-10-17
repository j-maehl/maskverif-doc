ni pmul (a b 7) {
   shared c r;
   var ij;

   r1 = $; r2 = $; r3 = $; r4 = $; r5 = $; r6 = $; r7 = $;

   c1 = a1 * b1;
   c2 = a2 * b2;
   c3 = a3 * b3;
   c4 = a4 * b4;
   c5 = a5 * b5;
   c6 = a6 * b6;
   c7 = a7 * b7;


   c1 = c1 + r1;
   c2 = c2 + r2;
   c3 = c3 + r3;
   c4 = c4 + r4;
   c5 = c5 + r5;
   c6 = c6 + r6;
   c7 = c7 + r7;

   ij = a1 * b7;
   c1 = c1 + ij;
   ij = a2 * b1;
   c2 = c2 + ij;
   ij = a3 * b2;
   c3 = c3 + ij;
   ij = a4 * b3;
   c4 = c4 + ij;
   ij = a5 * b4;
   c5 = c5 + ij;
   ij = a6 * b5;
   c6 = c6 + ij;
   ij = a7 * b6;
   c7 = c7 + ij;

   ij = a7 * b1;
   c1 = c1 + ij;
   ij = a1 * b2;
   c2 = c2 + ij;
   ij = a2 * b3;
   c3 = c3 + ij;
   ij = a3 * b4;
   c4 = c4 + ij;
   ij = a4 * b5;
   c5 = c5 + ij;
   ij = a5 * b6;
   c6 = c6 + ij;
   ij = a6 * b7;
   c7 = c7 + ij;



   c1 = c1 + r7;
   c2 = c2 + r1;
   c3 = c3 + r2;
   c4 = c4 + r3;
   c5 = c5 + r4;
   c6 = c6 + r5;
   c7 = c7 + r6;

   ij = a1 * b6;
   c1 = c1 + ij;
   ij = a2 * b7;
   c2 = c2 + ij;
   ij = a3 * b1;
   c3 = c3 + ij;
   ij = a4 * b2;
   c4 = c4 + ij;
   ij = a5 * b3;
   c5 = c5 + ij;
   ij = a6 * b4;
   c6 = c6 + ij;
   ij = a7 * b5;
   c7 = c7 + ij;

   ij = a6 * b1;
   c1 = c1 + ij;
   ij = a7 * b2;
   c2 = c2 + ij;
   ij = a1 * b3;
   c3 = c3 + ij;
   ij = a2 * b4;
   c4 = c4 + ij;
   ij = a3 * b5;
   c5 = c5 + ij;
   ij = a4 * b6;
   c6 = c6 + ij;
   ij = a5 * b7;
   c7 = c7 + ij;

   r1 = $; r2 = $; r3 = $; r4 = $; r5 = $; r6 = $; r7 = $;

   c1 = c1 + r1;
   c2 = c2 + r2;
   c3 = c3 + r3;
   c4 = c4 + r4;
   c5 = c5 + r5;
   c6 = c6 + r6;
   c7 = c7 + r7;

   ij = a1 * b5;
   c1 = c1 + ij;
   ij = a2 * b6;
   c2 = c2 + ij;
   ij = a3 * b7;
   c3 = c3 + ij;
   ij = a4 * b1;
   c4 = c4 + ij;
   ij = a5 * b2;
   c5 = c5 + ij;
   ij = a6 * b3;
   c6 = c6 + ij;
   ij = a7 * b4;
   c7 = c7 + ij;

   ij = a5 * b1;
   c1 = c1 + ij;
   ij = a6 * b2;
   c2 = c2 + ij;
   ij = a7 * b3;
   c3 = c3 + ij;
   ij = a1 * b4;
   c4 = c4 + ij;
   ij = a2 * b5;
   c5 = c5 + ij;
   ij = a3 * b6;
   c6 = c6 + ij;
   ij = a4 * b7;
   c7 = c7 + ij;
  
   c1 = c1 + r7;
   c2 = c2 + r1;
   c3 = c3 + r2;
   c4 = c4 + r3;
   c5 = c5 + r4;
   c6 = c6 + r5;
   c7 = c7 + r6;
   
   return c;
}
