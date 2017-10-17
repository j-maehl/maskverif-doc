require import Byte.

module M = {
  proc refresh (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr; r11 = $distr;

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

    a1 = a1 + r11;
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

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr; r11 = $distr;


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

    a1 = a1 + r10;
    a2 = a2 + r11;
    a3 = a3 + r1;
    a4 = a4 + r2;
    a5 = a5 + r3;
    a6 = a6 + r4;
    a7 = a7 + r5;
    a8 = a8 + r6;
    a9 = a9 + r7;
    a10 = a10 + r8;
    a11 = a11 + r9;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr; r11 = $distr;


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

    a1 = a1 + r8;
    a2 = a2 + r9;
    a3 = a3 + r10;
    a4 = a4 + r11;
    a5 = a5 + r1;
    a6 = a6 + r2;
    a7 = a7 + r3;
    a8 = a8 + r4;
    a9 = a9 + r5;
    a10 = a10 + r6;
    a11 = a11 + r7;



    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11);
  }

}.

masking sni 10 M.refresh Byte.ComRing.(+).