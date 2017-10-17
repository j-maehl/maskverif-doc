require import Byte.

module R = {
  proc refresh1 (x1:byte) : byte = { 
    return x1;
  }

  proc refresh2 (x1 x2:byte) : byte * byte = {
    var r : byte;
    r = $distr;
    x1 = x1 + r;
    x2 = x2 + r;
    return (x1, x2);
  }

  proc refresh3(x1 x2 x3:byte) : byte * byte * byte = {
    var r : byte;
    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;
    (* x1 = Refresh(x1); *)
    (* x2,x3 = Refresh(x2,x3) *)
    r = $distr;
    x2 = x2 + r;
    x3 = x3 + r;
    (* line 18 - 22 *)
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;

    return (x1,x2,x3);
  }

  proc refresh3C(x1 x2 x3:byte) : byte * byte * byte = {
    var r : byte;
    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x2 = x2 + r;
    (* x1 = Refresh(x1); *)
    (* x2,x3 = Refresh(x2,x3) *)
    r = $distr;
    x2 = x2 + r;
    x3 = x3 + r;
    (* line 18 - 22 *)
    r = $distr;
    x1 = x1 + r;
    x2 = x2 + r;

    return (x1,x2,x3);
  }
    
  proc refresh4(x1 x2 x3 x4:byte) = {
    var r: byte;
    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;
    r = $distr;
    x2 = x2 + r;
    x4 = x4 + r; 
    (* x1,x2 = Refresh(x1,x2); *)
 
    r = $distr;
    x1 = x1 + r;
    x2 = x2 + r;
    (* x3,x4 = Refresh(x3,x4) *)
    r = $distr;
    x3 = x3 + r;
    x4 = x4 + r;
    (* line 18 - 22 *)
(*    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;   
    r = $distr;
    x2 = x2 + r;
    x4 = x4 + r; *)
    return (x1,x2,x3,x4);
  }

 
  proc refresh4' (a1,a2,a3,a4) = {
    var r1, r2, r3, r4;
    var i1_4,i2_4,i3_4,i4_4,
        i1_5,i2_5,i3_5,i4_5 : byte;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr;
    
    i1_4 = a1+r1;
    i2_4 = a2+r2;
    i3_4 = a3+r3;
    i4_4 = a4+r4;
    i1_5 = i1_4+r4;
    i2_5 = i2_4+r1;
    i3_5 = i3_4+r2;
    i4_5 = i4_4+r3;

    return (i1_5, i2_5, i3_5, i4_5);
  }  

  proc refresh4_rot (a1,a2,a3,a4) = {
    var r1,r2,r3,r4;

    r1 = $ distr;
    r2 = $ distr;
    r3 = $ distr;
    r4 = $ distr;

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

  proc refresh5_rot (a1,a2,a3,a4,a5) = {
    var r1,r2,r3,r4,r5, r6, r7;

    r1 = $ distr;
    r2 = $ distr;
    r3 = $ distr;
    r4 = $ distr;
    r5 = $ distr;
    r6 = $ distr;
    r7 = $ distr;

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

    a1 = a1 + r6;
    a2 = a2 + r7; 
    a3 = a3 + r6;
    a4 = a4 + r7; 

    return (a1,a2,a3,a4,a5);
  } 

  proc refresh5_rot' (a1,a2,a3,a4,a5) = {
    var r1,r2,r3,r4,r5, r6;

    r1 = $ distr;
    r2 = $ distr;
    r3 = $ distr;
    r4 = $ distr;
    r5 = $ distr;
    r6 = $ distr;


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

    a1 = a1 + r6;
    a2 = a2 + r5;
    a3 = a3 + r1; 
    a4 = a4 + r2;
    a5 = a5 + r3;

    a1 = a1 + r3;
    a2 = a2 + r6;
    a3 = a3 + r5; 
    a4 = a4 + r1;
    a5 = a5 + r2;
   

    return (a1,a2,a3,a4,a5);
  } 

  proc refresh5C(x1 x2 x3 x4 x5) = {
    var r:byte;
    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r; 
    r = $distr;
    x2 = x2 + r;
    x4 = x4 + r; 
    (* x1,x2 = Refresh(x1,x2); *)
    r = $distr;
    x1 = x1 + r;
    x2 = x2 + r;
    (* x3,x4,x5 = Refresh(x3,x4,x5) *)
    r = $distr;
    x3 = x3 + r;
    x4 = x4 + r;
    r = $distr;
    x4 = x4 + r;
    x5 = x5 + r;
    r = $distr;
    x3 = x3 + r;
    x4 = x4 + r;
   
    (* line 18 - 22 *)
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;
    r = $distr;
    x2 = x2 + r;
    x4 = x4 + r;
    return (x1,x2,x3,x4, x5);
  }

  proc refresh5(x1 x2 x3 x4 x5) = {
    var r:byte;
    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;
    r = $distr;
    x2 = x2 + r;
    x4 = x4 + r;
    (* x1,x2 = Refresh(x1,x2); *)
    r = $distr;
    x1 = x1 + r;
    x2 = x2 + r;
    (* x3,x4,x5 = Refresh(x3,x4,x5) *)
    r = $distr;
    x3 = x3 + r;
    x5 = x5 + r;
    r = $distr;
    x4 = x4 + r;
    x5 = x5 + r;
    r = $distr;
    x3 = x3 + r;
    x5 = x5 + r;
    (* line 18 - 22 *)
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;
    r = $distr;
    x2 = x2 + r;
    x4 = x4 + r; 
    return (x1,x2,x3,x4,x5);
  }

  proc refresh6C(x1 x2 x3 x4 x5 x6) = {
    var r:byte;
    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x4 = x4 + r;
    r = $distr;
    x2 = x2 + r;
    x5 = x5 + r;
    r = $distr;
    x3 = x3 + r;
    x6 = x6 + r;
    (* x1,x2,x3 = Refresh3C(x1,x2,x3); *)
    r = $distr;
    x1 = x1 + r;
    x2 = x2 + r;
    r = $distr;
    x2 = x2 + r;
    x3 = x3 + r;
    r = $distr;
    x1 = x1 + r;
    x2 = x2 + r;
    (* x4,x5,x6 = Refresh3C(x4,x5,x6); *)
    r = $distr;
    x4 = x4 + r;
    x5 = x5 + r;
    r = $distr;
    x5 = x5 + r;
    x6 = x6 + r;
    r = $distr;
    x4 = x4 + r;
    x5 = x5 + r;

    (* line 18 - 22 *)
    r = $distr;
    x1 = x1 + r;
    x4 = x4 + r;
    r = $distr;
    x2 = x2 + r;
    x5 = x5 + r;
    r = $distr;
    x3 = x3 + r;
    x6 = x6 + r;  

    return (x1,x2,x3,x4,x5,x6);    
  }

  proc refresh6(x1 x2 x3 x4 x5 x6) = {
    var r:byte;
    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x4 = x4 + r;
    r = $distr;
    x2 = x2 + r;
    x5 = x5 + r;
    r = $distr;
    x3 = x3 + r;
    x6 = x6 + r; 
    (* x1,x2,x3 = Refresh(x1,x2,x3); *)
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;
    r = $distr;
    x2 = x2 + r;
    x3 = x3 + r;
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;
    (* x4,x5,x6 = Refresh(x4,x5,x6); *)
    r = $distr;
    x4 = x4 + r;
    x6 = x6 + r;
    r = $distr;
    x5 = x5 + r;
    x6 = x6 + r;
    r = $distr;
    x4 = x4 + r;
    x6 = x6 + r;
    (* line 18 - 22 *)
    r = $distr;
    x1 = x1 + r;
    x4 = x4 + r;
    r = $distr;
    x2 = x2 + r;
    x5 = x5 + r;
    r = $distr;
    x3 = x3 + r;
    x6 = x6 + r;  

    return (x1,x2,x3,x4,x5,x6);    
  }

  proc refresh6_rot(a1 a2 a3 a4 a5 a6) = {
    var r1,r2,r3,r4,r5,r6,r7,r8,r9:byte;
    r1 = $ distr;
    r2 = $ distr;
    r3 = $ distr;
    r4 = $ distr;
    r5 = $ distr;
    r6 = $ distr;
    r7 = $ distr;
    r8 = $ distr;
    r9 = $ distr;

    a1 = a1 + r1;
    a2 = a2 + r2;
    a3 = a3 + r3; 
    a4 = a4 + r4; 
    a5 = a5 + r5; 
    a6 = a6 + r6; 

    a1 = a1 + r6; 
    a2 = a2 + r1; 
    a3 = a3 + r2; 
    a4 = a4 + r3; 
    a5 = a5 + r4; 
    a6 = a6 + r5; 

    a1 = a1 + r7;
    a4 = a4 + r7;

    a2 = a2 + r8;
    a5 = a5 + r8; 
  
    a3 = a3 + r9;
    a4 = a4 + r9; 

    return (a1,a2,a3,a4,a5,a6);    
  }

  proc refresh7(x1 x2 x3 x4 x5 x6 x7) = {
    var r:byte;
    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x4 = x4 + r;
    r = $distr;
    x2 = x2 + r;
    x5 = x5 + r;
    r = $distr;
    x3 = x3 + r;
    x6 = x6 + r;
    (* x1,x2,x3 = Refresh(x1,x2,x3); *)
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;
    r = $distr;
    x2 = x2 + r;
    x3 = x3 + r;
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;
    (* x4,x5,x6,x7 = Refresh4(x4,x5,x6,x7); *)
    r = $distr;
    x4 = x4 + r;
    x6 = x6 + r;
    r = $distr;
    x5 = x5 + r;
    x7 = x7 + r;
    r = $distr;
    x4 = x4 + r;
    x5 = x5 + r;
    r = $distr;
    x6 = x6 + r;
    x7 = x7 + r;
    r = $distr;
    x4 = x4 + r;
    x6 = x6 + r;
    r = $distr;
    x5 = x5 + r;
    x7 = x7 + r;
    (* line 18 - 22 *)
    r = $distr;
    x1 = x1 + r;
    x4 = x4 + r;
    r = $distr;
    x2 = x2 + r;
    x5 = x5 + r;
    r = $distr;
    x3 = x3 + r;
    x6 = x6 + r;  

    return (x1,x2,x3,x4,x5,x6,x7);    
  }

  proc refresh7'(x1 x2 x3 x4 x5 x6 x7) = {
    var r:byte;
    var r1, r2, r3, r4;
    var i1_4,i2_4,i3_4,i4_4: byte;

    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x4 = x4 + r;
    r = $distr;
    x2 = x2 + r;
    x5 = x5 + r;
    r = $distr;
    x3 = x3 + r;
    x6 = x6 + r;
    (* x1,x2,x3 = Refresh(x1,x2,x3); *)
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;
    r = $distr;
    x2 = x2 + r;
    x3 = x3 + r;
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;
    (* x4,x5,x6,x7 = Refresh4(x4,x5,x6,x7); *)
    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr;
    
    i1_4 = x4+r1;
    i2_4 = x5+r2;
    i3_4 = x6+r3;
    i4_4 = x7+r4;
    x4 = i1_4+r4;
    x5 = i2_4+r1;
    x6 = i3_4+r2;
    x7 = i4_4+r3;

    (* line 18 - 22 *)
    r = $distr;
    x1 = x1 + r;
    x4 = x4 + r;
    r = $distr;
    x2 = x2 + r;
    x5 = x5 + r;
    r = $distr;
    x3 = x3 + r;
    x6 = x6 + r;  

    return (x1,x2,x3,x4,x5,x6,x7);    
  }

  proc refresh7_rot(a1 a2 a3 a4 a5 a6 a7) = {
    var r1,r2,r3,r4,r5,r6,r7,r8,r9,r10:byte;
    r1 = $ distr;
    r2 = $ distr;
    r3 = $ distr;
    r4 = $ distr;
    r5 = $ distr;
    r6 = $ distr;
    r7 = $ distr;
    r8 = $ distr;
    r9 = $ distr;
    r10 = $ distr;

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

    a1 = a1 + r8;
    a4 = a4 + r8;

    a2 = a2 + r9;
    a5 = a5 + r9; 
  
    a3 = a3 + r10;
    a6 = a6 + r10; 

    return (a1,a2,a3,a4,a5,a6,a7);    
  }

  proc refresh8(x1 x2 x3 x4 x5 x6 x7 x8) = {
    var r:byte;
    var r1, r2, r3, r4:byte;
    var i1_4,i2_4,i3_4,i4_4:byte;
    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x5 = x5 + r;
    r = $distr;
    x2 = x2 + r;
    x6 = x6 + r;    
    r = $distr;
    x3 = x3 + r;
    x7 = x7 + r;    
    r = $distr;
    x4 = x4 + r;
    x8 = x8 + r;    
    (* x1,x2,x3,x4 = Refresh4'(x1,x2,x3,x4) *)
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;
    r = $distr;
    x2 = x2 + r;
    x4 = x4 + r;
    r = $distr;
    x1 = x1 + r;
    x2 = x2 + r;
    r = $distr;
    x3 = x3 + r;
    x4 = x4 + r;
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;
    r = $distr;
    x2 = x2 + r;
    x4 = x4 + r;
    (* x5,x6,x7,x8 = Refresh4'(x5,x6,x7,x8) *)
    r = $distr;
    x5 = x5 + r;
    x7 = x7 + r;
    r = $distr;
    x6 = x6 + r;
    x8 = x8 + r;
    r = $distr;
    x5 = x5 + r;
    x6 = x6 + r;
    r = $distr;
    x7 = x7 + r;
    x8 = x8 + r;
    r = $distr;
    x5 = x5 + r;
    x7 = x7 + r;
    r = $distr;
    x6 = x6 + r;
    x8 = x8 + r;

    (* line 18 - 22 *)
    r = $distr;
    x1 = x1 + r;
    x5 = x5 + r;
    r = $distr;
    x2 = x2 + r;
    x6 = x6 + r;    
    r = $distr;
    x3 = x3 + r;
    x7 = x7 + r;    
    r = $distr;
    x4 = x4 + r;
    x8 = x8 + r;  

    return (x1,x2,x3,x4,x5,x6,x7,x8);
    
  }

 proc refresh8''(x1 x2 x3 x4 x5 x6 x7 x8) = {
    var r:byte;
    var r1, r2, r3, r4:byte;
    var i1_4,i2_4,i3_4,i4_4:byte;
    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x5 = x5 + r;
    r = $distr;
    x2 = x2 + r;
    x6 = x6 + r;    
    r = $distr;
    x3 = x3 + r;
    x7 = x7 + r;    
    r = $distr;
    x4 = x4 + r;
    x8 = x8 + r;    
    (* x1,x2,x3,x4 = Refresh4'(x1,x2,x3,x4) *)
    r = $distr;
    x1 = x1 + r;
    x2 = x2 + r;
    r = $distr;
    x3 = x3 + r;
    x4 = x4 + r;
    r = $distr;
    x1 = x1 + r;
    x3 = x3 + r;
    r = $distr;
    x2 = x2 + r;
    x4 = x4 + r;   
    (* x5,x6,x7,x8 = Refresh4'(x5,x6,x7,x8) *)
  
    r = $distr;
    x5 = x5 + r;
    x6 = x6 + r;
    r = $distr;
    x7 = x7 + r;
    x8 = x8 + r;
    r = $distr;
    x5 = x5 + r;
    x7 = x7 + r;
    r = $distr;
    x6 = x6 + r;
    x8 = x8 + r; 

    (* line 18 - 22 *)
(*    r = $distr;
    x1 = x1 + r;
    x5 = x5 + r;
    r = $distr;
    x2 = x2 + r;
    x6 = x6 + r;    
    r = $distr;
    x3 = x3 + r;
    x7 = x7 + r;    
    r = $distr;
    x4 = x4 + r;
    x8 = x8 + r;  *)

    return (x1,x2,x3,x4,x5,x6,x7,x8);
    
  }


  proc refresh8'(x1 x2 x3 x4 x5 x6 x7 x8) = {
    var r:byte;
    var r1, r2, r3, r4:byte;
    var i1_4,i2_4,i3_4,i4_4:byte;
    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x5 = x5 + r;
    r = $distr;
    x2 = x2 + r;
    x6 = x6 + r;    
    r = $distr;
    x3 = x3 + r;
    x7 = x7 + r;    
    r = $distr;
    x4 = x4 + r;
    x8 = x8 + r;    
    (* x1,x2,x3,x4 = Refresh4'(x1,x2,x3,x4) *)
    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr;
    i1_4 = x1+r1;
    i2_4 = x2+r2;
    i3_4 = x3+r3;
    i4_4 = x4+r4;
    x1 = i1_4+r4;
    x2 = i2_4+r1;
    x3 = i3_4+r2;
    x4 = i4_4+r3;
    (* x5,x6,x7,x8 = Refresh4'(x5,x6,x7,x8) *)
    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr;
    i1_4 = x5+r1;
    i2_4 = x6+r2;
    i3_4 = x7+r3;
    i4_4 = x8+r4;
    x5 = i1_4+r4;
    x6 = i2_4+r1;
    x7 = i3_4+r2;
    x8 = i4_4+r3;
    (* line 18 - 22 *)
 (*   r = $distr;
    x1 = x1 + r;
    x5 = x5 + r;
    r = $distr;
    x2 = x2 + r;
    x6 = x6 + r;    
    r = $distr;
    x3 = x3 + r;
    x7 = x7 + r;    
    r = $distr;
    x4 = x4 + r;
    x8 = x8 + r;  *)

    return (x1,x2,x3,x4,x5,x6,x7,x8);
    
  }

  

  proc refresh9 (x1 x2 x3 x4 x5 x6 x7 x8 x9) = {

    var r,r1,r2,r3,r4,r5,r6,r7:byte;

    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x5 = x5 + r;
    r = $distr;
    x2 = x2 + r;
    x6 = x6 + r;
    r = $distr;
    x3 = x3 + r;
    x7 = x7 + r;
    r = $distr;
    x4 = x4 + r;
    x8 = x8 + r;

    (* x1,....x4 = refresh5C(x1,...x4) *)
    r1 = $ distr;
    r2 = $ distr;
    r3 = $ distr;
    r4 = $ distr;

    x1 = x1 + r1;
    x2 = x2 + r2;
    x3 = x3 + r3; 
    x4 = x4 + r4; 

    x1 = x1 + r4; 
    x2 = x2 + r1; 
    x3 = x3 + r2; 
    x4 = x4 + r3; 
    (* x5, .... x9 = refresh6C(x5,....x9) *)

    r1 = $ distr;
    r2 = $ distr;
    r3 = $ distr;
    r4 = $ distr;
    r5 = $ distr;
    r6 = $ distr;
    r7 = $ distr;

    x5 = x5 + r1;
    x6 = x6 + r2;
    x7 = x7 + r3; 
    x8 = x8 + r4; 
    x9 = x9 + r5; 

    x5 = x5 + r5; 
    x6 = x6 + r1; 
    x7 = x7 + r2; 
    x8 = x8 + r3; 
    x9 = x9 + r4; 

    x5 = x5 + r6;
    x6 = x6 + r7; 
    x7 = x7 + r6;
    x8 = x8 + r7;     

    (* line 18 - 22 *)
    r = $distr;
    x1 = x1 + r;
    x5 = x5 + r;
    r = $distr;
    x2 = x2 + r;
    x6 = x6 + r;
    r = $distr;
    x3 = x3 + r;
    x7 = x7 + r;
    r = $distr;
    x4 = x4 + r;
    x8 = x8 + r;

    return (x1,x2,x3,x4,x5,x6,x7,x8,x9);
  }

 proc refresh10(x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = {

    var r,r1,r2,r3,r4,r5,r6,r7,r8,r9:byte;

    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x6 = x6 + r;
    r = $distr;
    x2 = x2 + r;
    x7 = x7 + r;
    r = $distr;
    x3 = x3 + r;
    x8 = x8 + r;
    r = $distr;
    x4 = x4 + r;
    x9 = x9 + r;
    r = $distr;
    x5 = x5 + r;
    x10 = x10 + r;
    (* x1,....x5 = refresh5C(x1,...x5) *)
    r1 = $ distr;
    r2 = $ distr;
    r3 = $ distr;
    r4 = $ distr;
    r5 = $ distr;
    r6 = $ distr;
    r7 = $ distr;

    x1 = x1 + r1;
    x2 = x2 + r2;
    x3 = x3 + r3; 
    x4 = x4 + r4; 
    x5 = x5 + r5; 

    x1 = x1 + r5; 
    x2 = x2 + r1; 
    x3 = x3 + r2; 
    x4 = x4 + r3; 
    x5 = x5 + r4; 

    x1 = x1 + r6;
    x2 = x2 + r7; 
    x3 = x3 + r6;
    x4 = x4 + r7; 
    (* x6, .... x10 = refresh6C(x6,....x10) *)
    r1 = $ distr;
    r2 = $ distr;
    r3 = $ distr;
    r4 = $ distr;
    r5 = $ distr;
    r6 = $ distr;
    r7 = $ distr;

    x6 = x6 + r1;
    x7 = x7 + r2;
    x8 = x8 + r3; 
    x9 = x9 + r4; 
    x10 = x10 + r5; 

    x6 = x6 + r5; 
    x7 = x7 + r1; 
    x8 = x8 + r2; 
    x9 = x9 + r3; 
    x10 = x10 + r4; 

    x6 = x6 + r6;
    x7 = x7 + r7; 
    x8 = x8 + r6;
    x9 = x9 + r7; 

    (* line 18 - 22 *)
    r = $distr;
    x1 = x1 + r;
    x6 = x6 + r;
    r = $distr;
    x2 = x2 + r;
    x7 = x7 + r;
    r = $distr;
    x3 = x3 + r;
    x8 = x8 + r;
    r = $distr;
    x4 = x4 + r;
    x9 = x9 + r;
    r = $distr;
    x5 = x5 + r;
    x10 = x10 + r;

    return (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10);
  }

  proc refresh11(x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = {

    var r,r1,r2,r3,r4,r5,r6,r7,r8,r9:byte;

    (* line 8 - 12 *)
    r = $distr;
    x1 = x1 + r;
    x6 = x6 + r;
    r = $distr;
    x2 = x2 + r;
    x7 = x7 + r;
    r = $distr;
    x3 = x3 + r;
    x8 = x8 + r;
    r = $distr;
    x4 = x4 + r;
    x9 = x9 + r;
    r = $distr;
    x5 = x5 + r;
    x10 = x10 + r;
    (* x1,....x5 = refresh5C(x1,...x5) *)
    r1 = $ distr;
    r2 = $ distr;
    r3 = $ distr;
    r4 = $ distr;
    r5 = $ distr;
    r6 = $ distr;
    r7 = $ distr;

    x1 = x1 + r1;
    x2 = x2 + r2;
    x3 = x3 + r3; 
    x4 = x4 + r4; 
    x5 = x5 + r5; 

    x1 = x1 + r5; 
    x2 = x2 + r1; 
    x3 = x3 + r2; 
    x4 = x4 + r3; 
    x5 = x5 + r4; 

    x1 = x1 + r6;
    x2 = x2 + r7; 
    x3 = x3 + r6;
    x4 = x4 + r7; 
   
    (* x6, .... x11 = refresh6C(x6,....x11) *)

    r1 = $ distr;
    r2 = $ distr;
    r3 = $ distr;
    r4 = $ distr;
    r5 = $ distr;
    r6 = $ distr;
    r7 = $ distr;
    r8 = $ distr;
    r9 = $ distr;

    x6 = x6 + r1;
    x7 = x7 + r2;
    x8 = x8 + r3; 
    x9 = x9 + r4; 
    x10 = x10 + r5; 
    x11 = x11 + r6; 

    x6 = x6 + r6; 
    x7 = x7 + r1; 
    x8 = x8 + r2; 
    x9 = x9 + r3; 
    x10 = x10 + r4; 
    x11 = x11 + r5; 

    x6 = x6 + r7;
    x9 = x9 + r7;

    x7 = x7 + r8;
    x10 = x10 + r8; 
  
    x8 = x8 + r9;
    x11 = x11 + r9; 

    

    (* line 18 - 22 *)
    r = $distr;
    x1 = x1 + r;
    x6 = x6 + r;
    r = $distr;
    x2 = x2 + r;
    x7 = x7 + r;
    r = $distr;
    x3 = x3 + r;
    x8 = x8 + r;
    r = $distr;
    x4 = x4 + r;
    x9 = x9 + r;
    r = $distr;
    x5 = x5 + r;
    x10 = x10 + r;

    return (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11);
  }

}.

(*masking sni 2 R.refresh3 Byte.ComRing.(+).
masking sni 3 R.refresh4_rot Byte.ComRing.(+).
masking sni 3 R.refresh4 Byte.ComRing.(+).
masking sni 3 R.refresh4' Byte.ComRing.(+). 
masking sni 4 R.refresh5_rot' Byte.ComRing.(+).
masking sni 4 R.refresh5C Byte.ComRing.(+).
masking sni 4 R.refresh5 Byte.ComRing.(+). 
masking sni 5 R.refresh6_rot Byte.ComRing.(+). 
masking sni 5 R.refresh6 Byte.ComRing.(+). *)

masking sni 6 R.refresh7_rot Byte.ComRing.(+).
masking sni 6 R.refresh7 Byte.ComRing.(+).
masking sni 6 R.refresh7' Byte.ComRing.(+).
masking sni 7 R.refresh8'' Byte.ComRing.(+). 
masking sni 7 R.refresh8 Byte.ComRing.(+).
masking sni 7 R.refresh8' Byte.ComRing.(+). 
masking sni 8 R.refresh9 Byte.ComRing.(+).  

masking sni 9 R.refresh10 Byte.ComRing.(+). 
*)
masking sni 10 R.refresh11 Byte.ComRing.(+).

(a7 + r7, 
r4, 
a3 + r3 + r2, 
a2 + r2 + r1,
a1 + r1 + r7 + r8, 
a4 + r4 + r3 + r8) 
