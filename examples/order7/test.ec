require import Byte.

module M = {

 (* output = a1 + r15 + r122 + r16 + r113; a2 + r22 + r15 + r23 + r123; a3 + r32 +
                                                                    r22 + r33 +
                                                                    r16; 
a4 + r42 + r32 + r43 + r23; a5 + r52 + r42 + r53 + r33; a6 + r62 + r52 + r63 +
                                                        r43; a7 + r72 + r62 +
                                                             r73 + r53; 
a8 + r82 + r72 + r83 + r63; a9 + r92 + r82 + r93 + r73; a10 + r102 + r92 +
                                                        r103 + r83; a11 +
                                                                    r112 +
                                                                    r102 +
                                                                    r113 +
                                                                    r93; 
a12 + r122 + r112 + r123 + r103; 
123,741,215,136 tuples to check
can cot check (r16, r22, r23, a10 + r102 + r92 + r103, a8 + r82,
               a9 + r92 + r82 + r93, a6 + r62 + r52 + r63 + r43,
               a11 + r112 + r102 + r113 + r93,
               a12 + r122 + r112 + r123 + r103, a2 + r22 + r15 + r23 + r123,
               a1 + r15 + r122 + r16 + r113) reduce to (r16, r22, r23,
                                                        a10 + r102 + r92 +
                                                        r103, a8 + r82,
                                                        a9 + r92 + r82 + r93,
                                                        r43,
                                                        a11 + r112 + r102 +
                                                        r113 + r93,
                                                        a12 + r122 + r112 +
                                                        r123 + r103,
                                                        a2 + r22 + r15 + r23 +
                                                        r123,
                                                        a1 + r15 + r122 + r16 +
                                                        r113)

(r16, 
 r22, 
 r23, 
 a10 + r102 + r92 + r103, 
 a8 + r82,
 a9 + r92 + r82 + r93, 
 a6 + r62 + r52 + r63 + r43,
 a11 + r112 + r102 + r113 + r93,
               a12 + r122 + r112 + r123 + r103, a2 + r22 + r15 + r23 + r123,
               a1 + r15 + r122 + r16 + r113) reduce to 

(r1', 
 r2, 
 r2',
 r4,
 a8 + r8,
 a9 + r9 + r8 + r9',
 a10 + r10 + r9 + r10', 
 a11 + r11 + r10 + r11' + r9',
 a12 + r12 + r11 + r12' + r10',
 a1 + r1 + r122 + r1' + r11'
 a2 + r2 + r1 + r2' + r12')









*)
  proc refresh_12_2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr; r11 = $distr; r12 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;
    a9 = a9 + r9;
    a10 = a10 + r10;
    a11 = a11 + r11;
    a12 = a12 + r12;

    a1 = a1 + r12;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;
    a9 = a9 + r8;
    a10 = a10 + r9;
    a11 = a11 + r10;
    a12 = a12 + r11;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr; r11 = $distr; r12 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;
    a9 = a9 + r9;
    a10 = a10 + r10;
    a11 = a11 + r11;
    a12 = a12 + r12;

    a1 = a1 + r11;
    a2 = a2 + r12;
    a3 = a3 + r1;
    a4 = a4 + r2;
    a5 = a5 + r3;
    a6 = a6 + r4;
    a7 = a7 + r5;
    a8 = a8 + r6;
    a9 = a9 + r7;
    a10 = a10 + r8;
    a11 = a11 + r9;
    a12 = a12 + r10;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12);
  }

}.


masking sni 11 M.refresh_12_2 Byte.ComRing.(+).
