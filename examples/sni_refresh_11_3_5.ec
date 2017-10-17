require import Distr.
type K.
op ( + ) : K -> K -> K.
op distr : K distr.


module M = {
  proc refresh(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = {
    var r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11;
    var s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11;
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
    
    a0 = a0 + r0;
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
    
    a0 = a0 + r9;
    a1 = a1 + r10;
    a2 = a2 + r11;
    a3 = a3 + r0;
    a4 = a4 + r1;
    a5 = a5 + r2;
    a6 = a6 + r3;
    a7 = a7 + r4;
    a8 = a8 + r5;
    a9 = a9 + r6;
    a10 = a10 + r7;
    a11 = a11 + r8;
    
    
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
    
    a0 = a0 + s0;
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
    
    a0 = a0 + s7;
    a1 = a1 + s8;
    a2 = a2 + s9;
    a3 = a3 + s10;
    a4 = a4 + s11;
    a5 = a5 + s0;
    a6 = a6 + s1;
    a7 = a7 + s2;
    a8 = a8 + s3;
    a9 = a9 + s4;
    a10 = a10 + s5;
    a11 = a11 + s6;
    
    
    return (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
  }
}.

masking sni 11 M.refresh (+).


(a0 + r01 + r91, 
 a11 + r111 + r81, 
 a8 + r81 + r51,
 a4 + r41 + r13, 
 

 

 a2 + r21 + r111 + s21, 
 a7 + r71 + r41 + s71 + s21, 
 a0 + r01 + r91 + s01 + s71,
 a5 + r51 + r21 + s51 + s01,
 a10 + r101 + r71 + s101 + s51, 
 s101,
a1 + r13 + r101
a4 + r41 + r13, 
 

 )
