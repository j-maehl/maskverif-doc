require import Byte.

module M = {

   (* Marche pas: (a5 + r52, a6 + r62 + r52 + r63, r12, r13,
               a1 + r12 + r82 + r13 + r83, a7 + r72 + r62 + r73 + r63,
               a8 + r82 + r72 + r83 + r73) reduce to (a5 + r52,
                                                      a6 + r62 + r52 + r63,
                                                      r12, r13,
                                                      a1 + r12 + r82 + r13 +
                                                      r83,
                                                      a7 + r72 + r62 + r73 +
                                                      r63,
                                                      a8 + r82 + r72 + r83 +
                                                      r73) 
   ( 
    r12, 
    r13,
    a5 + r52,
    a6 + r62 + r52 + r63,
    a7 + r72 + r62 + r73 + r63,
    a8 + r82 + r72 + r83 + r73)
    a1 + r12 + r82 + r13 + r83,
*)
   proc refresh_8_1 (a1,a2,a3,a4,a5,a6,a7,a8) = {
    var r1, r2, r3, r4, r5, r6, r7, r8;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r8;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r8;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;

    return (a1,a2,a3,a4,a5,a6,a7,a8);
  }
   (* Ok *)
   proc refresh_8_2 (a1,a2,a3,a4,a5,a6,a7,a8) = {
    var r1, r2, r3, r4, r5, r6, r7, r8;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r8;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r7;
    a2 = a2 + r8;
    a3 = a3 + r1;
    a4 = a4 + r2;
    a5 = a5 + r3;
    a6 = a6 + r4;
    a7 = a7 + r5;
    a8 = a8 + r6;

    return (a1,a2,a3,a4,a5,a6,a7,a8);
  }

  (* ok *)
  proc refresh_8_3 (a1,a2,a3,a4,a5,a6,a7,a8) = {
    var r1, r2, r3, r4, r5, r6, r7, r8;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r8;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r6;
    a2 = a2 + r7;
    a3 = a3 + r8;
    a4 = a4 + r1;
    a5 = a5 + r2;
    a6 = a6 + r3;
    a7 = a7 + r4;
    a8 = a8 + r5;

    return (a1,a2,a3,a4,a5,a6,a7,a8);
  }

  (* Marche pas *)
(* can cot check (r12, a8 + r82 + r72, a7 + r72 + r62, a6 + r62 + r52, a4 + r42,
               a5 + r52 + r42 + r53 + r13, a1 + r12 + r82 + r13 + r53) reduce to (
r12, a8 + r82 + r72, a7 + r72 + r62, a6 + r62 + r52, a4 + r42,
a5 + r52 + r42 + r53 + r13, a1 + r12 + r82 + r13 + r53) 

r12, 
a1 + r1 + r8 + r'1 + r'5,
a4 + r4,
a5 + r5 + r4 + r'5 + r'1,
a6 + r6 + r5,                 
a7 + r7 + r6, 
a8 + r8 + r7, 

*) 
  proc refresh_8_4 (a1,a2,a3,a4,a5,a6,a7,a8) = {
    var r1, r2, r3, r4, r5, r6, r7, r8;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r8;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r5;
    a2 = a2 + r6;
    a3 = a3 + r7;
    a4 = a4 + r8;
    a5 = a5 + r1;
    a6 = a6 + r2;
    a7 = a7 + r3;
    a8 = a8 + r4;

    return (a1,a2,a3,a4,a5,a6,a7,a8);
  }

 (* Ok *)
 proc refresh_8_5 (a1,a2,a3,a4,a5,a6,a7,a8) = {
    var r1, r2, r3, r4, r5, r6, r7, r8;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r8;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r4;
    a2 = a2 + r5;
    a3 = a3 + r6;
    a4 = a4 + r7;
    a5 = a5 + r8;
    a6 = a6 + r1;
    a7 = a7 + r2;
    a8 = a8 + r3;

    return (a1,a2,a3,a4,a5,a6,a7,a8);
  }

  (* Ok *)
  proc refresh_8_6 (a1,a2,a3,a4,a5,a6,a7,a8) = {
    var r1, r2, r3, r4, r5, r6, r7, r8;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r8;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r3;
    a2 = a2 + r4;
    a3 = a3 + r5;
    a4 = a4 + r6;
    a5 = a5 + r7;
    a6 = a6 + r8;
    a7 = a7 + r1;
    a8 = a8 + r2;

    return (a1,a2,a3,a4,a5,a6,a7,a8);
  }

  (* Marche pas *)
  (* can cot check (r72, a7 + r72 + r62 + r73, a6 + r62 + r52 + r63 + r73,
               a5 + r52 + r42 + r53 + r63, a4 + r42 + r32 + r43 + r53,
               a3 + r32, r43) reduce to (r72, a7 + r72 + r62 + r73,
                                         a6 + r62 + r52 + r63 + r73,
                                         a5 + r52 + r42 + r53 + r63,
                                         a4 + r42 + r32 + r43 + r53,
                                         a3 + r32, r43)
 r7, 
 r'4,
 a3 + r3, 
 a4 + r4 + r3 + r'4 + r'5,
 a5 + r5 + r4 + r'5 + r'6, 
 a6 + r6 + r5 + r'6 + r'7,
 a7 + r7 + r6 + r'7


 *)
  proc refresh_8_7 (a1,a2,a3,a4,a5,a6,a7,a8) = {
    var r1, r2, r3, r4, r5, r6, r7, r8;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r8;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r2;
    a2 = a2 + r3;
    a3 = a3 + r4;
    a4 = a4 + r5;
    a5 = a5 + r6;
    a6 = a6 + r7;
    a7 = a7 + r8;
    a8 = a8 + r1;

    return (a1,a2,a3,a4,a5,a6,a7,a8);
  }

  (* Marche pas (a7 + r72 + r52 + r73 + r33, a6 + r62 + r42 + r63 + r23,
               a5 + r52 + r32 + r53 + r13, a4 + r42 + r22 + r43 + r83,
               a3 + r32 + r12 + r33 + r73, a2 + r22 + r82 + r23 + r63,
               a1 + r12 + r72 + r13 + r53) *)
  proc refresh_8_2_4 (a1,a2,a3,a4,a5,a6,a7,a8) = {
    var r1, r2, r3, r4, r5, r6, r7, r8;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r7;
    a2 = a2 + r8;
    a3 = a3 + r1;
    a4 = a4 + r2;
    a5 = a5 + r3;
    a6 = a6 + r4;
    a7 = a7 + r5;
    a8 = a8 + r6;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;

    a1 = a1 + r5;
    a2 = a2 + r6;
    a3 = a3 + r7;
    a4 = a4 + r8;
    a5 = a5 + r1;
    a6 = a6 + r2;
    a7 = a7 + r3;
    a8 = a8 + r4;

    return (a1,a2,a3,a4,a5,a6,a7,a8);
  }
  (* ok *)
  proc refresh_9_2 (a1,a2,a3,a4,a5,a6,a7,a8,a9) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;
    a9 = a9 + r9;

    a1 = a1 + r9;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;
    a9 = a9 + r8;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;
    a9 = a9 + r9;

    a1 = a1 + r8;
    a2 = a2 + r9;
    a3 = a3 + r1;
    a4 = a4 + r2;
    a5 = a5 + r3;
    a6 = a6 + r4;
    a7 = a7 + r5;
    a8 = a8 + r6;
    a9 = a9 + r7;
  
    return (a1,a2,a3,a4,a5,a6,a7,a8,a9);
  }

  (* Marche *)
  proc refresh_9_3 (a1,a2,a3,a4,a5,a6,a7,a8,a9) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;
    a9 = a9 + r9;

    a1 = a1 + r9;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;
    a9 = a9 + r8;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;
    a9 = a9 + r9;

    a1 = a1 + r7;
    a2 = a2 + r8;
    a3 = a3 + r9;
    a4 = a4 + r1;
    a5 = a5 + r2;
    a6 = a6 + r3;
    a7 = a7 + r4;
    a8 = a8 + r5;
    a9 = a9 + r6;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9);
  }
  
  (* Marche *)
  proc refresh_9_4 (a1,a2,a3,a4,a5,a6,a7,a8,a9) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;
    a9 = a9 + r9;

    a1 = a1 + r9;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;
    a9 = a9 + r8;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;
    a9 = a9 + r9;

    a1 = a1 + r6;
    a2 = a2 + r7;
    a3 = a3 + r8;
    a4 = a4 + r9;
    a5 = a5 + r1;
    a6 = a6 + r2;
    a7 = a7 + r3;
    a8 = a8 + r4;
    a9 = a9 + r5;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9);
  }

  (* Marche *)
  proc refresh_9_5 (a1,a2,a3,a4,a5,a6,a7,a8,a9) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;
    a9 = a9 + r9;

    a1 = a1 + r9;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;
    a9 = a9 + r8;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;
    a9 = a9 + r9;

    a1 = a1 + r5;
    a2 = a2 + r6;
    a3 = a3 + r7;
    a4 = a4 + r8;
    a5 = a5 + r9;
    a6 = a6 + r1;
    a7 = a7 + r2;
    a8 = a8 + r3;
    a9 = a9 + r4;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9);
  }

  proc refresh_9_6 (a1,a2,a3,a4,a5,a6,a7,a8,a9) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;
    a9 = a9 + r9;

    a1 = a1 + r9;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;
    a9 = a9 + r8;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;
    a9 = a9 + r9;

    a1 = a1 + r4;
    a2 = a2 + r5;
    a3 = a3 + r6;
    a4 = a4 + r7;
    a5 = a5 + r8;
    a6 = a6 + r9;
    a7 = a7 + r1;
    a8 = a8 + r2;
    a9 = a9 + r3;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9);
  }

  (* Ok *)
  proc refresh_9_7 (a1,a2,a3,a4,a5,a6,a7,a8,a9) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr;
    

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;
    a9 = a9 + r9;

    a1 = a1 + r9;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;
    a9 = a9 + r8;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3;
    a4 = a4 + r4;
    a5 = a5 + r5;
    a6 = a6 + r6;
    a7 = a7 + r7;
    a8 = a8 + r8;
    a9 = a9 + r9;

    a1 = a1 + r3;
    a2 = a2 + r4;
    a3 = a3 + r5;
    a4 = a4 + r6;
    a5 = a5 + r7;
    a6 = a6 + r8;
    a7 = a7 + r9;
    a8 = a8 + r1;
    a9 = a9 + r2;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9);
  }

  (* Ok *)
  proc refresh_10_2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9, r10;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr;
    

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

    a1 = a1 + r10;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;
    a9 = a9 + r8;
    a10 = a10 + r9;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr;

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

    a1 = a1 + r9;
    a2 = a2 + r10;
    a3 = a3 + r1;
    a4 = a4 + r2;
    a5 = a5 + r3;
    a6 = a6 + r4;
    a7 = a7 + r5;
    a8 = a8 + r6;
    a9 = a9 + r7;
    a10 = a10 + r8;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
  }

  (* Ok *)
  proc refresh_10_3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9, r10;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr;
    

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

    a1 = a1 + r10;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;
    a9 = a9 + r8;
    a10 = a10 + r9;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr;

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

    a1 = a1 + r8;
    a2 = a2 + r9;
    a3 = a3 + r10;
    a4 = a4 + r1;
    a5 = a5 + r2;
    a6 = a6 + r3;
    a7 = a7 + r4;
    a8 = a8 + r5;
    a9 = a9 + r6;
    a10 = a10 + r7;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
  }

  (* marche *)
  proc refresh_10_4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9, r10;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr;
    

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

    a1 = a1 + r10;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;
    a9 = a9 + r8;
    a10 = a10 + r9;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr;

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

    a1 = a1 + r7;
    a2 = a2 + r8;
    a3 = a3 + r9;
    a4 = a4 + r10;
    a5 = a5 + r1;
    a6 = a6 + r2;
    a7 = a7 + r3;
    a8 = a8 + r4;
    a9 = a9 + r5;
    a10 = a10 + r6;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
  }

  (* marche pas 
(r2, 
 a10 + r10, 
 r5, 
 r7, 
a6 + r6 + r5  + r'6 + r'1
a7 + r7 + r6  + r'7 + r'2,
a10 + r10, 
a1 + r1 + r10 + r'1 + r'6,
a2 + r2 + r1  + r'2 + r'7,)
*)
  proc refresh_10_5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9, r10;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr;
    

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

    a1 = a1 + r10;
    a2 = a2 + r1;
    a3 = a3 + r2;
    a4 = a4 + r3;
    a5 = a5 + r4;
    a6 = a6 + r5;
    a7 = a7 + r6;
    a8 = a8 + r7;
    a9 = a9 + r8;
    a10 = a10 + r9;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr;

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

    a1 = a1 + r6;
    a2 = a2 + r7;
    a3 = a3 + r8;
    a4 = a4 + r9;
    a5 = a5 + r10;
    a6 = a6 + r1;
    a7 = a7 + r2;
    a8 = a8 + r3;
    a9 = a9 + r4;
    a10 = a10 + r5;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
  }

  (* Marche pas (r15, r22, r23, a9 + r92 + r82 + r93, a7 + r72,
               a8 + r82 + r72 + r83, a11 + r112 + r102 + r113 + r93,
               a10 + r102 + r92 + r103 + r83, a2 + r22 + r14 + r23 + r113,
               a1 + r14 + r112 + r15 + r103)

(r'1, 
 r2, 
 r'2, 
a7  + r7,
a8  + r8  + r7  + r'8,
a9  + r9  + r8  + r'9, 
a10 + r10 + r9  + r'10 + r'8, 
a11 + r11 + r10 + r'11 + r'9,
a1  + r1  + r11 + r'1  + r'10
a2  + r2  + r1  + r'2  + r'11,)
 


*)
  proc refresh_11_2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) = {
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

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11);
  }
  (* Marche *)
  proc refresh_11_3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) = {
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

    a1 = a1 + r9;
    a2 = a2 + r10;
    a3 = a3 + r11;
    a4 = a4 + r1;
    a5 = a5 + r2;
    a6 = a6 + r3;
    a7 = a7 + r4;
    a8 = a8 + r5;
    a9 = a9 + r6;
    a10 = a10 + r7;
    a11 = a11 + r8;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11);
  }

  (* Ok *)
  proc refresh_11_4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) = {
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


  (* marche pas *)
  proc refresh_12_3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) = {
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



    a1 = a1 + r10;
    a2 = a2 + r11;
    a3 = a3 + r12;
    a4 = a4 + r1;
    a5 = a5 + r2;
    a6 = a6 + r3;
    a7 = a7 + r4;
    a8 = a8 + r5;
    a9 = a9 + r6;
    a10 = a10 + r7;
    a11 = a11 + r8;
    a12 = a12 + r9;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12);
  }


(* 
can cot check (a6 + r62 + r52, r53, a11 + r112, r72, a4 + r42 + r32,
               a3 + r32 + r22, a1 + r15 + r122, a2 + r22 + r15 + r23,
               a12 + r122 + r112 + r123 + r73, a7 + r72 + r62 + r73 + r23,
               a5 + r52 + r42 + r53 + r123) reduce to (a6 + r62 + r52, r53,
                                                       a11 + r112, r72,
                                                       a4 + r42 + r32,
                                                       a3 + r32 + r22,
                                                       a1 + r15 + r122,
                                                       a2 + r22 + r15 + r23,
                                                       a12 + r122 + r112 +
                                                       r123 + r73,
                                                       a7 + r72 + r62 + r73 +
                                                       r23,
                                                       a5 + r52 + r42 + r53 +
                                                       r123)
NOT SNI 


r'5, 
r7, 
a11 + r1, 
a12 + r12 + r11 + r'12 + r'7, 
a1 + r1 + r12, 
a2  + r2  + r1  + r'2,
a3 + r3 + r2, 
a4 + r4 + r3,               
a5  + r5  + r4  + r'5 + r'12               
a6 + r6 + r5, 
a7  + r7  + r6  + r'7 + r'2,

*)
  proc refresh_12_5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) = {
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



    a1 = a1 + r8;
    a2 = a2 + r9;
    a3 = a3 + r10;
    a4 = a4 + r11;
    a5 = a5 + r12;
    a6 = a6 + r1;
    a7 = a7 + r2;
    a8 = a8 + r3;
    a9 = a9 + r4;
    a10 = a10 + r5;
    a11 = a11 + r6;
    a12 = a12 + r7;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12);
  }


(* 
(a12 + r122, r23, r112, r62, r16, a7 + r72,
               a6 + r62 + r42 + r63 + r16, a9 + r92 + r72 + r93 + r43,
               a11 + r112 + r92 + r113 + r63, a4 + r42 + r22 + r43 + r113,
               a2 + r22 + r122 + r23 + r93) reduce to (a12 + r122, r23, r112,
                                                       r62, r16, a7 + r72,
                                                       a6 + r62 + r42 + r63 +
                                                       r16,
                                                       a9 + r92 + r72 + r93 +
                                                       r43,
                                                       a11 + r112 + r92 +
                                                       r113 + r63,
                                                       a4 + r42 + r22 + r43 +
                                                       r113,
                                                       a2 + r22 + r122 + r23 +
                                                       r93)

r'2, 
r'1, 
a12 + r12, 
a2 + r2 + r12 + r'2 + r'9
a4 + r4 + r2  + r'4 + r'11,
a6 + r6 + r4  + r'6 + r'1,
r6, 
a7  + r7,
a9  + r9  + r7 + r'9 + r'4,
a11 + r11 + r9 + r'11 + r'6, 
r11, 
*)

 proc refresh_12_2_5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) = {
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



    a1 = a1 + r8;
    a2 = a2 + r9;
    a3 = a3 + r10;
    a4 = a4 + r11;
    a5 = a5 + r12;
    a6 = a6 + r1;
    a7 = a7 + r2;
    a8 = a8 + r3;
    a9 = a9 + r4;
    a10 = a10 + r5;
    a11 = a11 + r6;
    a12 = a12 + r7;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12);
  }

  (* Ok *)
  proc refresh_12_1_2_3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) = {
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



    a1 = a1 + r10;
    a2 = a2 + r11;
    a3 = a3 + r12;
    a4 = a4 + r1;
    a5 = a5 + r2;
    a6 = a6 + r3;
    a7 = a7 + r4;
    a8 = a8 + r5;
    a9 = a9 + r6;
    a10 = a10 + r7;
    a11 = a11 + r8;
    a12 = a12 + r9;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12);
  }

  (* can cot check (a13 + r132 + r122, r17, r22, r23, a10 + r102 + r92,
               a9 + r92 + r82 + r93, a7 + r72, a8 + r82 + r72 + r83,
               a12 + r122 + r112 + r123 + r93,
               a11 + r112 + r102 + r113 + r83, a2 + r22 + r16 + r23 + r123,
               a1 + r16 + r132 + r17 + r113) reduce to (a13 + r132 + r122,
                                                        r17, r22, r23,
                                                        a10 + r102 + r92,
                                                        a9 + r92 + r82 + r93,
                                                        a7 + r72,
                                                        a8 + r82 + r72 + r83,
                                                        a12 + r122 + r112 +
                                                        r123 + r93,
                                                        a11 + r112 + r102 +
                                                        r113 + r83,
                                                        a2 + r22 + r16 + r23 +
                                                        r123,
                                                        a1 + r16 + r132 + r17 +
                                                        r113) 

  
a13 + r132 + r122, 
r17, 
r22, 
r23, 

a10 + r102 + r92,
         
a9 + r92 + r82 + r93, 
a7 + r72, a8 + r82 + r72 + r83,
a11 + r112 + r102 + r113 + r83,                
a12 + r122 + r112 + r123 + r93,           
a1 + r16 + r132 + r17 + r113
a2 + r22 + r16 + r23 + r123,
  *)
  
  proc refresh_13_3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr; r11 = $distr; r12 = $distr;r13 = $distr;
    

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
    a13 = a13 + r13;

    a1 = a1 + r13;
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
    a13 = a13 + r12;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr; r11 = $distr; r12 = $distr; r13 = $distr;

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
    a13 = a13 + r13;



    a1 = a1 + r11;
    a2 = a2 + r12;
    a3 = a3 + r13;
    a4 = a4 + r1;
    a5 = a5 + r2;
    a6 = a6 + r3;
    a7 = a7 + r4;
    a8 = a8 + r5;
    a9 = a9 + r6;
    a10 = a10 + r7;
    a11 = a11 + r8;
    a12 = a12 + r9;
    a13 = a13 + r10;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13);
  }

 (* can cot check (a13 + r132 + r122, r17, r22, r23, a10 + r102 + r92,
               a9 + r92 + r82 + r93, a7 + r72, a8 + r82 + r72 + r83,
               a12 + r122 + r112 + r123 + r93,
               a11 + r112 + r102 + r113 + r83, a2 + r22 + r16 + r23 + r123,
               a1 + r16 + r132 + r17 + r113) reduce to (a13 + r132 + r122,
                                                        r17, r22, r23,
                                                        a10 + r102 + r92,
                                                        a9 + r92 + r82 + r93,
                                                        a7 + r72,
                                                        a8 + r82 + r72 + r83,
                                                        a12 + r122 + r112 +
                                                        r123 + r93,
                                                        a11 + r112 + r102 +
                                                        r113 + r83,
                                                        a2 + r22 + r16 + r23 +
                                                        r123,
                                                        a1 + r16 + r132 + r17 +
                                                        r113) *)

 proc refresh_13_4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr; r11 = $distr; r12 = $distr;r13 = $distr;
    

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
    a13 = a13 + r13;

    a1 = a1 + r13;
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
    a13 = a13 + r12;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr; r11 = $distr; r12 = $distr; r13 = $distr;

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
    a13 = a13 + r13;



    a1 = a1 + r10;
    a2 = a2 + r11;
    a3 = a3 + r12;
    a4 = a4 + r13;
    a5 = a5 + r1;
    a6 = a6 + r2;
    a7 = a7 + r3;
    a8 = a8 + r4;
    a9 = a9 + r5;
    a10 = a10 + r6;
    a11 = a11 + r7;
    a12 = a12 + r8;
    a13 = a13 + r9;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12, a13);
  }

  (* 
can cot check (a7 + r72 + r62, a6 + r62 + r52, r53, a12 + r122, r82,
               a4 + r42 + r32, a2 + r22 + r16, a1 + r16 + r132,
               a3 + r32 + r22 + r33, a13 + r132 + r122 + r133 + r83,
               a8 + r82 + r72 + r83 + r33, a5 + r52 + r42 + r53 + r133) reduce to (
a7 + r72 + r62, a6 + r62 + r52, r53, a12 + r122, r82, a4 + r42 + r32,
a2 + r22 + r16, a1 + r16 + r132, a3 + r32 + r22 + r33,
a13 + r132 + r122 + r133 + r83, a8 + r82 + r72 + r83 + r33,
a5 + r52 + r42 + r53 + r133) 


(a7 + r72 + r62, 
 a6 + r62 + r52, 
 r53, 
 a12 + r122, 
 r82,
 a4 + r42 + r32, 
 a2 + r22 + r16, 
 a1 + r16 + r132,

 a3  + r32  + r22  + r33, 
 a8  + r82  + r72  + r83  + r33, 
 a13 + r132 + r122 + r133 + r83,
 a5  + r52  + r42  + r53  + r133

1         i       i           i
2         i
3     i                           i
4         i                   
5     +                i      +
6         i       +
7         i       +               i
8     +                i
9                      i      +   
10                     i          i
11                     i          i
12        i            i          i
13    +           +           +   i  

*)

 proc refresh_13_5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr; r11 = $distr; r12 = $distr;r13 = $distr;
    

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
    a13 = a13 + r13;

    a1 = a1 + r13;
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
    a13 = a13 + r12;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr; r11 = $distr; r12 = $distr; r13 = $distr;

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
    a13 = a13 + r13;



    a1 = a1 + r9;
    a2 = a2 + r10;
    a3 = a3 + r11;
    a4 = a4 + r12;
    a5 = a5 + r13;
    a6 = a6 + r1;
    a7 = a7 + r2;
    a8 = a8 + r3;
    a9 = a9 + r4;
    a10 = a10 + r5;
    a11 = a11 + r6;
    a12 = a12 + r7;
    a13 = a13 + r8;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12, a13);
  }

(* can cot check (a4 + r42 + r22, a8 + r82 + r62, a3, a13 + r132 + r112, r102,
               a9 + r92, a1, a11 + r112 + r92 + r113, r103,
               a10 + r102 + r82 + r103 + r63, a6 + r62 + r42 + r63 + r23,
               a2 + r22 + r132 + r23 + r113) reduce to (a4 + r42 + r22,
                                                        a8 + r82 + r62, a3,
                                                        a13 + r132 + r112,
                                                        r102, a9 + r92, a1,
                                                        a11 + r112 + r92 +
                                                        r113, r103,
                                                        a10 + r102 + r82 +
                                                        r103 + r63,
                                                        a6 + r62 + r42 + r63 +
                                                        r23,
                                                        a2 + r22 + r132 + r23 +
                                                        r113) 



 1      i             
 2      i   i
 3   +      i    
 4     
 5
 6               
 7      i        
 8   +           
 9      i           
 10     i
 11  i              
 12     i          
 13 i+          
)

*)

 proc refresh_13_2_4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr; r11 = $distr; r12 = $distr;r13 = $distr;
    

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
    a13 = a13 + r13;

    a1 = a1 + r12;
    a2 = a2 + r13;
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
    a13 = a13 + r11;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; 
    r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;
    r9 = $distr; r10 = $distr; r11 = $distr; r12 = $distr; r13 = $distr;

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
    a13 = a13 + r13;



    a1 = a1 + r10;
    a2 = a2 + r11;
    a3 = a3 + r12;
    a4 = a4 + r13;
    a5 = a5 + r1;
    a6 = a6 + r2;
    a7 = a7 + r3;
    a8 = a8 + r4;
    a9 = a9 + r5;
    a10 = a10 + r6;
    a11 = a11 + r7;
    a12 = a12 + r8;
    a13 = a13 + r9;

    return (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12, a13);
  }


(* refresh_13_2_5 
(a13 + r132, 
 r72, 
 a8 + r82, 
 a3 + r32 + r16, 
 a1 + r16 + r122,
 r22, 
 a5  + r52  + r32  + r53,
 a10 + r102 + r82  + r103 + r53,
 a2  + r22  + r132 + r23  + r103, 
 a7  + r72  + r52  + r73  + r23, 
 a12 + r122 + r102 + r123 + r73, 
 r123) 

1         i
2   +     i
3         i
4  
5   i 
6  
7   +     i
8         i
9   
10  +
11  
12 i+
13        i  

*)


(* masking sni 7 M.refresh Byte.ComRing.(+). *)
(* masking sni 7 M.refresh_8_1 Byte.ComRing.(+). *)
(* masking sni 7 M.refresh_8_2 Byte.ComRing.(+).  *)
(* masking sni 7 M.refresh_8_3 Byte.ComRing.(+). *)
(* masking sni 7 M.refresh_8_4 Byte.ComRing.(+). *)
(* masking sni 7 M.refresh_8_5 Byte.ComRing.(+). *)
(* masking sni 7 M.refresh_8_6 Byte.ComRing.(+). *)
(* masking sni 7 M.refresh_8_7 Byte.ComRing.(+). *)
(* masking sni 7 M.refresh_8_2_4 Byte.ComRing.(+). *)
(* masking sni 8 M.refresh_9 Byte.ComRing.(+). *)
(* masking sni 8 M.refresh_9_2 Byte.ComRing.(+). *)
(* masking sni 8 M.refresh_9_3 Byte.ComRing.(+). *)
(* masking sni 8 M.refresh_9_4 Byte.ComRing.(+). *)
(* masking sni 8 M.refresh_9_5 Byte.ComRing.(+). *)
(* masking sni 8 M.refresh_9_6 Byte.ComRing.(+). *)
(* masking sni 8 M.refresh_9_7 Byte.ComRing.(+). *)

(* masking sni 9 M.refresh_10_2 Byte.ComRing.(+). *)
(* masking sni 9 M.refresh_10_3 Byte.ComRing.(+). *)
(* masking sni 9 M.refresh_10_4 Byte.ComRing.(+). *)
(* masking sni 9 M.refresh_10_5 Byte.ComRing.(+). *)

(* masking sni 10 M.refresh_11_2 Byte.ComRing.(+). *)
(* masking sni 10 M.refresh_11_3 Byte.ComRing.(+). *)
(* masking sni 10 M.refresh_11_4 Byte.ComRing.(+). *)

(* masking sni 11 M.refresh_12_2 Byte.ComRing.(+).  *)

(* masking sni 11 M.refresh_12 Byte.ComRing.(+). *)

(* masking sni 11 M.refresh_12_5 Byte.ComRing.(+). *)
(* masking sni 11 M.refresh_12_2_5 Byte.ComRing.(+). *)
(*masking sni 11 M.refresh_12_1_2_3 Byte.ComRing.(+). *)
(* masking sni 12 M.refresh_13_4 Byte.ComRing.(+). *)



(* Il y a un vrai pb avec 12 *) 

(* Doit marcher avec 3 tours 
0       +
1               +
2           +
3               +
4       +
5               +
6           +
7
8       +
9
10          +
11

*)

(*
12 5
0       +
1          i 
2          i
3       i   
4          i
5       +  
6          
7
8       
9          i
10      +  
11         i

5 obs,
3 out 
reste 6 obs -> 5 cases


*)

(* marche pas 
13 5
0       +
1           i
2       i
3           i
4           i
5       +  
6           
7       
8       
9           i    
10      + 
11          i
12          i

5 obs,
3 out 
reste 7 obs -> 6 cases
*)

(*  ca doit etre ok
13 4
0       i
1           i
2           i
3           i
4       +    
5         
6           
7                   Il faudrait un i ici pour bloquer la reduction
8       +
9           i    
10          i 
11          i
12      +

5 obs,
3 out 
reste 7 obs -> 6 cases
*)



(* 
12 : 3 4
0       +
1       +  
2           i       
3           i  
4       +     
5       +      
6           i 
7           i  
8       +     
9       +     
10            
11      

marche pas
*)

(* 
12 :  2 5
0       +
1            i
2                  
3       i    i  
4           
5       +    i 
6            
7             
8            
9            
10      +       
11      

reste 6 obs
marche pas
*)
 
(* 13 3
0 
1
2
3
4
5
6     
7    i
8    i
9    i
10  i+
11  i+
12  i+

9 obs 6 internes

0   +
1       i
2   i
3   +
4   
5   
6   +   
7       i   
8       i
9   +  
10      i
11      i
12  +

5 output + 2 internes
reste 5 
*) 