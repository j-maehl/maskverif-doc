require import Byte.

module Mult = {
  proc refresh_r (a1,a2,a3) = { 
    var r1, r2, r3;
    var i1_1, i2_1, i3_1,
        i1_2, i2_2, i3_2,
        i1_3, i2_3, i3_3,
        i1_4, i2_4, i3_4,
        i1_5, i2_5, i3_5;
    r1 = $ distr; r2 = $ distr; r3 = $distr;

    (i1_1, i2_1, i3_1) = (a1,a2,a3);
    (i1_2, i2_2, i3_2) = (r1,r2,r3);
    (i1_3, i2_3, i3_3) = (r3,r1,r2);                 
    (i1_4, i2_4, i3_4) = (i1_1 + i1_2, i2_1 + i2_2, i3_1 + i3_2);
    (i1_5, i2_5, i3_5) = (i1_4 + i1_3, i2_4 + i2_3, i3_4 + i3_3);

    return (i1_5, i2_5, i3_5);
  }

  proc refresh (a) = {
   var a1, a2, a3, b1, b2, b3;

    a1 = $distr;
    a2 = $distr;
    a3 = a + a1 + a2;

    (b1, b2, b3) = refresh_r (a1,a2,a3); 

    return (b1,b2,b3);
  }

  proc mult_r (a1,a2,a3,b1,b2,b3) = {
    var i1_1,i2_1,i3_1, 
        i1_2,i2_2,i3_2,
        i1_3,i2_3,i3_3,
        i1_4,i2_4,i3_4,
        i1_5,i2_5,i3_5,
        i1_6,i2_6,i3_6,
        i1_7,i2_7,i3_7,
        i1_8,i2_8,i3_8, 
        i1_9,i2_9,i3_9,
        i1_10,i2_10,i3_10,
        i1_11,i2_11,i3_11,
        i1_12,i2_12,i3_12,
        i1_13,i2_13,i3_13 : byte;
    var r1, r2, r3;

    (* Blue *)
    (i1_1,i2_1,i3_1) = (a1,a2,a3);
    (i1_2,i2_2,i3_2) = (b1,b2,b3);
    (i1_3,i2_3,i3_3) = (b2,b3,b1);
    (i1_4,i2_4,i3_4) = (a2,a3,a1);
                      
    (* Red *)         
    (i1_5,i2_5,i3_5) = (i1_1*i1_2, i2_1 * i2_2, i3_1 * i3_2);
    (i1_6,i2_6,i3_6) = (i1_1*i1_3, i2_1 * i2_3, i3_1 * i3_3);
    (i1_7,i2_7,i3_7) = (i1_4*i1_2, i2_4 * i2_2, i3_4 * i3_2);

    (* Green *)       
    r1 = $distr; r2 = $distr; r3 = $distr;
    (i1_8,i2_8,i3_8) = (r1,r2,r3);
    (i1_9,i2_9,i3_9) = (r3,r1,r2);

    (i1_10,i2_10,i3_10) = (i1_5 +i1_8, i2_5 +i2_8, i3_5 +i3_8); 
    (i1_11,i2_11,i3_11) = (i1_10+i1_6, i2_10+i2_6, i3_10+i3_6); 
    (i1_12,i2_12,i3_12) = (i1_11+i1_7, i2_11+i2_7, i3_11+i3_7); 

    (i1_13,i2_13,i3_13) = (i1_12+i1_9, i2_12+i2_9, i3_12+i3_9); 

    return (i1_13,i2_13,i3_13);
  }

  proc mult (a,b) = {
    var a1,a2,a3,b1,b2,b3,c1,c2,c3;
    a1 = $distr;
    a2 = $distr;
    a3 = a + a1 + a2;

    b1 = $distr;
    b2 = $distr;
    b3 = b + b1 + b2;

    (c1,c2,c3) = mult_r(a1,a2,a3,b1,b2,b3);
    return (c1+c2+c3);
  }
      
  proc sbox3_r(a1,a2,a3,b1,b2,b3,c1,c2,c3) = {
    var i1_1,i2_1,i3_1,
        i1_2,i2_2,i3_2,
        i1_3,i2_3,i3_3,
        i1_4,i2_4,i3_4,
        i1_5,i2_5,i3_5,
        i1_6,i2_6,i3_6;
    
    (i1_1,i2_1,i3_1) = mult_r(a1,a2,a3,b1,b2,b3);
    (i1_2,i2_2,i3_2) = (i1_1+c1, i2_1+c2, i3_1+c3);  (* out a *)
    (i1_3,i2_3,i3_3) = mult_r(b1,b2,b3,c1,c2,c3);
    (i1_4,i2_4,i3_4) = (i1_3+a1, i2_3+a2, i3_3+a3);  (* out b *)
    (i1_5,i2_5,i3_5) = mult_r(c1,c2,c3,a1,a2,a3);
    (i1_6,i2_6,i3_6) = (i1_5+b1,i2_5+b2,i3_5+b3);    (* out c *)  

    return (i1_2,i2_2,i3_2,i1_4,i2_4,i3_4,i1_6,i2_6,i3_6);

  }

  proc sbox3_1(a,b,c) = {
    var a1,a2,a3, b1,b2,b3, c1,c2,c3;
    a1 = $distr;
    a2 = $distr;
    a3 = a + a1 + a2;

    b1 = $distr;
    b2 = $distr;
    b3 = b + b1 + b2;

    c1 = $distr;
    c2 = $distr;
    c3 = c + c1 + c2;

    (a1,a2,a3, b1,b2,b3, c1,c2,c3) = 
      sbox3_r (a1,a2,a3, b1,b2,b3, c1,c2,c3);

    return (a1 + a2 + a3, b1 + b2 + b3, c1 + c2 + c3);
  }

  proc sbox3_2(a,b,c) = {
    var a1,a2,a3, b1,b2,b3, c1,c2,c3;
    a1 = $distr;
    a2 = $distr;
    a3 = a + a1 + a2;

    b1 = $distr;
    b2 = $distr;
    b3 = b + b1 + b2;

    c1 = $distr;
    c2 = $distr;
    c3 = c + c1 + c2;

    (a1,a2,a3, b1,b2,b3, c1,c2,c3) = 
      sbox3_r (a1,a2,a3, b1,b2,b3, c1,c2,c3);
    (a1,a2,a3, b1,b2,b3, c1,c2,c3) = 
      sbox3_r (a1,a2,a3, b1,b2,b3, c1,c2,c3);

    return (a1 + a2 + a3, b1 + b2 + b3, c1 + c2 + c3);

  }

  proc sbox2_r(a1,a2,a3,b1,b2,b3) = {
    var c1, c2, c3, d1, d2, d3;
    (c1,c2,c3) = mult_r(a1,a2,a3,b1,b2,b3);
    (d1,d2,d3) = (a1 + c1,a2 + c2,a3 + c3);
    return (c1,c2,c3,d1,d2,d3);
  }

  proc sbox4_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) = {
    var e1, e2, e3, f1, f2, f3;
    (e1,e2,e3,f1,f2,f3) = sbox2_r(c1,c2,c3,d1,d2,d3);
    (e1,e2,e3) = (e1 + a1,e2 + a2,e3 + a3);
    (f1,f2,f3) = (f1 + b1,f2 + b2,f3 + b3);
    return (c1,c2,c3,d1,d2,d3,e1,e2,e3,f1,f2,f3);
  }

  proc sbox4_1(a,b,c,d) = {
    var a1,a2,a3, b1,b2,b3, c1,c2,c3, d1,d2,d3;
    a1 = $distr;
    a2 = $distr;
    a3 = a + a1 + a2;

    b1 = $distr;
    b2 = $distr;
    b3 = b + b1 + b2;

    c1 = $distr;
    c2 = $distr;
    c3 = c + c1 + c2;

    d1 = $distr;
    d2 = $distr;
    d3 = d + d1 + d2;

    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    return (a1 + a2 + a3,b1 + b2 + b3,c1 + c2 + c3,d1 + d2 + d3);
  }

  proc sbox4_2(a,b,c,d) = {
    var a1,a2,a3, b1,b2,b3, c1,c2,c3, d1,d2,d3;
    a1 = $distr;
    a2 = $distr;
    a3 = a + a1 + a2;

    b1 = $distr;
    b2 = $distr;
    b3 = b + b1 + b2;

    c1 = $distr;
    c2 = $distr;
    c3 = c + c1 + c2;

    d1 = $distr;
    d2 = $distr;
    d3 = d + d1 + d2;

    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);

    return (a1 + a2 + a3,b1 + b2 + b3,c1 + c2 + c3,d1 + d2 + d3);
  }

  proc sbox4_3(a,b,c,d) = {
    var a1,a2,a3, b1,b2,b3, c1,c2,c3, d1,d2,d3;
    a1 = $distr;
    a2 = $distr;
    a3 = a + a1 + a2;

    b1 = $distr;
    b2 = $distr;
    b3 = b + b1 + b2;

    c1 = $distr;
    c2 = $distr;
    c3 = c + c1 + c2;

    d1 = $distr;
    d2 = $distr;
    d3 = d + d1 + d2;

    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);

    return (a1 + a2 + a3,b1 + b2 + b3,c1 + c2 + c3,d1 + d2 + d3);
  }

  proc sbox2r_r(a1,a2,a3,b1,b2,b3) = {
    var c1, c2, c3, d1, d2, d3;
    (c1,c2,c3) = mult_r(a1,a2,a3,b1,b2,b3);
    (d1,d2,d3) = (a1 + c1,a2 + c2,a3 + c3);
    (d1,d2,d3) = refresh_r(d1,d2,d3);
    return (c1,c2,c3,d1,d2,d3);
  }

  proc sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) = {
    var e1, e2, e3, f1, f2, f3;
    (e1,e2,e3,f1,f2,f3) = sbox2r_r(c1,c2,c3,d1,d2,d3);
    (e1,e2,e3) = (e1 + a1,e2 + a2,e3 + a3);
    (f1,f2,f3) = (f1 + b1,f2 + b2,f3 + b3);
    return (c1,c2,c3,d1,d2,d3,e1,e2,e3,f1,f2,f3);
  }

  proc sbox4r_3(a,b,c,d) = {
    var a1,a2,a3, b1,b2,b3, c1,c2,c3, d1,d2,d3;
    a1 = $distr;
    a2 = $distr;
    a3 = a + a1 + a2;

    b1 = $distr;
    b2 = $distr;
    b3 = b + b1 + b2;

    c1 = $distr;
    c2 = $distr;
    c3 = c + c1 + c2;

    d1 = $distr;
    d2 = $distr;
    d3 = d + d1 + d2;

    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);

    return (a1 + a2 + a3,b1 + b2 + b3,c1 + c2 + c3,d1 + d2 + d3);
  }

  proc sbox4r_4(a,b,c,d) = {
    var a1,a2,a3, b1,b2,b3, c1,c2,c3, d1,d2,d3;
    a1 = $distr;
    a2 = $distr;
    a3 = a + a1 + a2;

    b1 = $distr;
    b2 = $distr;
    b3 = b + b1 + b2;

    c1 = $distr;
    c2 = $distr;
    c3 = c + c1 + c2;

    d1 = $distr;
    d2 = $distr;
    d3 = d + d1 + d2;

    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);

    return (a1 + a2 + a3,b1 + b2 + b3,c1 + c2 + c3,d1 + d2 + d3);
  }

  proc sbox4r_5(a,b,c,d) = {
    var a1,a2,a3, b1,b2,b3, c1,c2,c3, d1,d2,d3;
    a1 = $distr;
    a2 = $distr;
    a3 = a + a1 + a2;

    b1 = $distr;
    b2 = $distr;
    b3 = b + b1 + b2;

    c1 = $distr;
    c2 = $distr;
    c3 = c + c1 + c2;

    d1 = $distr;
    d2 = $distr;
    d3 = d + d1 + d2;

    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);

    return (a1 + a2 + a3,b1 + b2 + b3,c1 + c2 + c3,d1 + d2 + d3);
  }  

  proc sbox4r_6(a,b,c,d) = {
    var a1,a2,a3, b1,b2,b3, c1,c2,c3, d1,d2,d3;
    a1 = $distr;
    a2 = $distr;
    a3 = a + a1 + a2;

    b1 = $distr;
    b2 = $distr;
    b3 = b + b1 + b2;

    c1 = $distr;
    c2 = $distr;
    c3 = c + c1 + c2;

    d1 = $distr;
    d2 = $distr;
    d3 = d + d1 + d2;

    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);
    (a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3) =
      sbox4r_r(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3);

    return (a1 + a2 + a3,b1 + b2 + b3,c1 + c2 + c3,d1 + d2 + d3);
  }


   proc multp_r (r1,r2,r3, a1,a2,a3, b1,b2,b3) = {
    var i1_1,i2_1,i3_1, 
        i1_2,i2_2,i3_2,
        i1_3,i2_3,i3_3,
        i1_4,i2_4,i3_4,
        i1_5,i2_5,i3_5,
        i1_6,i2_6,i3_6,
        i1_7,i2_7,i3_7,
        i1_8,i2_8,i3_8, 
        i1_9,i2_9,i3_9,
        i1_10,i2_10,i3_10,
        i1_11,i2_11,i3_11,
        i1_12,i2_12,i3_12,
        i1_13,i2_13,i3_13 : byte;
    

    (* Blue *)
    (i1_1,i2_1,i3_1) = (a1,a2,a3);
    (i1_2,i2_2,i3_2) = (b1,b2,b3);
    (i1_3,i2_3,i3_3) = (b2,b3,b1);
    (i1_4,i2_4,i3_4) = (a2,a3,a1);
                      
    (* Red *)         
    (i1_5,i2_5,i3_5) = (i1_1*i1_2, i2_1 * i2_2, i3_1 * i3_2);
    (i1_6,i2_6,i3_6) = (i1_1*i1_3, i2_1 * i2_3, i3_1 * i3_3);
    (i1_7,i2_7,i3_7) = (i1_4*i1_2, i2_4 * i2_2, i3_4 * i3_2);

    (* Green *)       
    (i1_8,i2_8,i3_8) = (r1,r2,r3);
    (i1_9,i2_9,i3_9) = (r3,r1,r2);

    (i1_10,i2_10,i3_10) = (i1_5 +i1_8, i2_5 +i2_8, i3_5 +i3_8); 
    (i1_11,i2_11,i3_11) = (i1_10+i1_6, i2_10+i2_6, i3_10+i3_6); 
    (i1_12,i2_12,i3_12) = (i1_11+i1_7, i2_11+i2_7, i3_11+i3_7); 

    (i1_13,i2_13,i3_13) = (i1_12+i1_9, i2_12+i2_9, i3_12+i3_9); 

    return (i1_13,i2_13,i3_13);
  }

  proc mult4(a1,a2,a3, b1,b2,b3, c1,c2,c3, d1,d2,d3) = {
    var r1, r2, r3;
    var r1', r2', r3';
    var i1_1,i2_1,i3_1, 
        i1_2,i2_2,i3_2,
        i1_3,i2_3,i3_3;

    r1 = $distr; r2 = $distr; r3 = $distr;
    r1' = $distr; r2' = $distr; r3' = $distr;

    (i1_1,i2_1,i3_1) = multp_r(r1,r2,r3, a1,a2,a3, b1,b2,b3);
    (i1_2,i2_2,i3_2) = multp_r(r1',r2',r3', i1_1,i2_1,i3_1, c1,c2,c3);
    (i1_3,i2_3,i3_3) = multp_r(r1,r2,r3, i1_2,i2_2,i3_2, d1,d2,d3);
    return (i1_3,i2_3,i3_3);
  }

}.
