require import Byte.

module M = {
  proc refresh(b0:byte, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13) = {
    var (*a0,*) a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13;
    var r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13;
    var s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13;

    r0 <$ distr;
    r1 <$ distr;
    r2 <$ distr;
    r3 <$ distr;
    r4 <$ distr;
    r5 <$ distr;
    r6 <$ distr;
    r7 <$ distr;
    r8 <$ distr;
    r9 <$ distr;
    r10 <$ distr;
    r11 <$ distr;
    r12 <$ distr;
    r13 <$ distr;
    
(*    a0 = r0 + r13; *)
(*    a0 = zeror; *)
    a1 = r1 + r0;
    a2 = r2 + r1;
    a3 = r3 + r2;
    a4 = r4 + r3;
    a5 = r5 + r4;
    a6 = r6 + r5;
    a7 = r7 + r6;
    a8 = r8 + r7;
    a9 = r9 + r8;
    a10 = r10 + r9;
    a11 = r11 + r10;
    a12 = r12 + r11;
    a13 = r13 + r12;
    
    
    s0 <$ distr;
    s1 <$ distr;
    s2 <$ distr;
    s3 <$ distr;
    s4 <$ distr;
    s5 <$ distr;
    s6 <$ distr;
    s7 <$ distr;
    s8 <$ distr;
    s9 <$ distr;
    s10 <$ distr;
    s11 <$ distr;
    s12 <$ distr;
    s13 <$ distr;
    
(*    a0 = a0 + s0; *)
    a1 = a1 + s1;
    a2 = a2 + s2;
    a3 = a3 + s3;
    a4 = a4 + s4;
    a5 = a5 + s5;
    a6 = a6 + s6;
    a7 = a7 + s7;
    a8 = a8 + s8;
    a9 = a9 + s9;
    a10 = a10 + s10;
    a11 = a11 + s11;
    a12 = a12 + s12;
    a13 = a13 + s13;
    
(*    a0 = a0 + s11; *)
    a1 = a1 + s12;
    a2 = a2 + s13;
    a3 = a3 + s0;
    a4 = a4 + s1;
    a5 = a5 + s2;
    a6 = a6 + s3;
    a7 = a7 + s4;
    a8 = a8 + s5;
    a9 = a9 + s6;
    a10 = a10 + s7;
    a11 = a11 + s8;
    a12 = a12 + s9;
    a13 = a13 + s10;
    
    
(*    a0 = a0 + b0; *)
    a1 = a1 + b1;
    a2 = a2 + b2;
    a3 = a3 + b3;
    a4 = a4 + b4;
    a5 = a5 + b5;
    a6 = a6 + b6;
    a7 = a7 + b7;
    a8 = a8 + b8;
    a9 = a9 + b9;
    a10 = a10 + b10;
    a11 = a11 + b11;
    a12 = a12 + b12;
    a13 = a13 + b13;
    
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
  }
}.

masking sni 13 M.refresh Byte.ComRing.(+).
