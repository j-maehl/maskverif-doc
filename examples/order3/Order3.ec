require import Byte.

module Mult = {
  proc refresh_r (a1,a2,a3,a4) = {
    var r1, r2, r3, r4;
    var i1_1,i2_1,i3_1,i4_1,
        i1_2,i2_2,i3_2,i4_2,
        i1_3,i2_3,i3_3,i4_3,
        i1_4,i2_4,i3_4,i4_4,
        i1_5,i2_5,i3_5,i4_5 : byte;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr;


    (i1_2,i2_2,i3_2,i4_2) = (r1,r2,r3,r4);
    (i1_3,i2_3,i3_3,i4_3) = (r4,r1,r2,r3);
                     
    (i1_4,i2_4,i3_4,i4_4) = (a1+i1_2, a2+i2_2, a3+i3_2, a4+i4_2);
    (i1_5,i2_5,i3_5,i4_5) = (i1_4+i1_3, i2_4+i2_3, i3_4+i3_3, i4_4+i4_3);

    return (i1_5, i2_5, i3_5, i4_5);
  }

  proc refresh(a) = {
    var a1, a2, a3, a4;

    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = a + a1 + a2 + a3;

    (a1,a2,a3,a4) = refresh_r(a1,a2,a3,a4);
    return (a1 + a2 + a3 + a4);
  }

  proc mult_r (a1,a2,a3,a4,b1,b2,b3,b4) = {
    var r1, r2, r3, r4;
    var i1_1,i2_1,i3_1,i4_1,
        i1_2,i2_2,i3_2,i4_2,
        i1_3,i2_3,i3_3,i4_3,
        i1_4,i2_4,i3_4,i4_4,
        i1_5,i2_5,i3_5,i4_5,
        i1_6,i2_6,i3_6,i4_6,
        i1_7,i2_7,i3_7,i4_7,
        i1_8,i2_8,i3_8,i4_8, 
        i1_9,i2_9,i3_9,i4_9,
        i1_10,i2_10,i3_10,i4_10,
        i1_11,i2_11,i3_11,i4_11,
        i1_12,i2_12,i3_12,i4_12,
        i1_13,i2_13,i3_13,i4_13,
        i1_14,i2_14,i3_14,i4_14,
        i1_15,i2_15,i3_15,i4_15,
        i1_16,i2_16,i3_16,i4_16,
        i1_17,i2_17,i3_17,i4_17,
        i1_18,i2_18,i3_18,i4_18,
        i1_19,i2_19,i3_19,i4_19,
        i1_20,i2_20,i3_20,i4_20 : byte;

    (* Blue *)
    (i1_1,i2_1,i3_1,i4_1) = (a1,a2,a3,a4);
    (i1_2,i2_2,i3_2,i4_2) = (b1,b2,b3,b4);
    (i1_3,i2_3,i3_3,i4_3) = (b2,b3,b4,b1);
    (i1_4,i2_4,i3_4,i4_4) = (a2,a3,a4,a1);
    (i1_5,i2_5,i3_5,i4_5) = (b3,b4,b1,b2);
                         
    (* Red *) 
    (i1_6,i2_6,i3_6,i4_6) = (i1_1 * i1_2, i2_1 * i2_2, i3_1 * i3_2, i4_1 * i4_2);
    (i1_7,i2_7,i3_7,i4_7) = (i1_1 * i1_3, i2_1 * i2_3, i3_1 * i3_3, i4_1 * i4_3);
    (i1_8,i2_8,i3_8,i4_8) = (i1_4 * i1_2, i2_4 * i2_2, i3_4 * i3_2, i4_4 * i4_2);
    (i1_9,i2_9,i3_9,i4_9) = (i1_1 * i1_5, i2_1 * i2_5, i3_1 * i3_5, i4_1 * i4_5);

    (* Green *)       
    r1 = $distr; r2 = $distr; r3 = $distr; r4 = $distr;
    (i1_10,i2_10,i3_10,i4_10) = (r1,r2,r3,r4);
    (i1_11,i2_11,i3_11,i4_11) = (r2,r3,r4,r1); 

    (i1_14,i2_14,i3_14,i4_14) = (i1_6 +i1_10, i2_6 +i2_10, i3_6 +i3_10, i4_6 +i4_10);
    (i1_15,i2_15,i3_15,i4_15) = (i1_14+i1_7 , i2_14+i2_7 , i3_14+i3_7 , i4_14+i4_7 );
    (i1_16,i2_16,i3_16,i4_16) = (i1_15+i1_8 , i2_15+i2_8 , i3_15+i3_8 , i4_15+i4_8 );
    (i1_17,i2_17,i3_17,i4_17) = (i1_16+i1_11, i2_16+i2_11, i3_16+i3_11, i4_16+i4_11); 
    (i1_18,i2_18,i3_18,i4_18) = (i1_17+i1_9 , i2_17+i2_9 , i3_17+i3_9 , i4_17+i4_9 );

    return (i1_18,i2_18,i3_18,i4_18);
  }

  proc mult (a,b) = {
    var a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = a + a1 + a2 + a3;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = b + b1 + b2 + b3;

    (c1,c2,c3,c4) = mult_r(a1,a2,a3,a4,b1,b2,b3,b4);
    return (c1+c2+c3+c4);
  }

  proc rmult_r (a1,a2,a3,a4, b1,b2,b3,b4) = {
    var i1_1,i2_1,i3_1,i4_1,
        i1_2,i2_2,i3_2,i4_2;
    (i1_1,i2_1,i3_1,i4_1) = mult_r(a1,a2,a3,a4, b1,b2,b3,b4);
    (i1_2,i2_2,i3_2,i4_2) = refresh_r(i1_1,i2_1,i3_1,i4_1);
    return (i1_2,i2_2,i3_2,i4_2);
  }

  proc multrr_r (a1,a2,a3,a4,b1,b2,b3,b4) = {
    var c1,c2,c3,c4;
    (a1,a2,a3,a4) = refresh_r(a1,a2,a3,a4); 
    (b1,b2,b3,b4) = refresh_r(b1,b2,b3,b4); 
    (c1,c2,c3,c4) = mult_r(a1,a2,a3,a4,b1,b2,b3,b4);
    return (c1,c2,c3,c4);
  }

  proc multr_r (a1,a2,a3,a4,b1,b2,b3,b4) = {
    var c1,c2,c3,c4;
    (b1,b2,b3,b4) = refresh_r(b1,b2,b3,b4) ; 
    (c1,c2,c3,c4) = mult_r(a1,a2,a3,a4,b1,b2,b3,b4);
    return (c1,c2,c3,c4);
  }

  proc sbox3_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4) = {
    var i1_1,i2_1,i3_1,i4_1,
        i1_2,i2_2,i3_2,i4_2,
        i1_3,i2_3,i3_3,i4_3,
        i1_4,i2_4,i3_4,i4_4,
        i1_5,i2_5,i3_5,i4_5,
        i1_6,i2_6,i3_6,i4_6;
    
    (i1_1,i2_1,i3_1,i4_1) = mult_r(a1,a2,a3,a4,b1,b2,b3,b4);
    (i1_2,i2_2,i3_2,i4_2) = (i1_1+c1, i2_1+c2, i3_1+c3, i4_1+c4);  (* out a *)
    (i1_3,i2_3,i3_3,i4_3) = mult_r(b1,b2,b3,b4,c1,c2,c3,c4);
    (i1_4,i2_4,i3_4,i4_4) = (i1_3+a1, i2_3+a2, i3_3+a3, i4_3+a4);  (* out b *)
    (i1_5,i2_5,i3_5,i4_5) = mult_r(c1,c2,c3,c4,a1,a2,a3,a4);
    (i1_6,i2_6,i3_6,i4_6) = (i1_5+b1, i2_5+b2, i3_5+b3, i4_5+b4);  (* out c *)  

    return (i1_2,i2_2,i3_2,i4_2,i1_4,i2_4,i3_4,i4_4,i1_6,i2_6,i3_6,i4_6);

  }

  proc sbox3_1(a,b,c) = {
    var a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = a + a1 + a2 + a3;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = b + b1 + b2 + b3;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = c + c1 + c2 + c3;

    (a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4) = 
      sbox3_r (a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4);

    return (a1 + a2 + a3 + a4, b1 + b2 + b3 + b4, c1 + c2 + c3 + c4);
  }

  proc sbox3_2(a,b,c) = {
    var a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = a + a1 + a2 + a3;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = b + b1 + b2 + b3;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = c + c1 + c2 + c3;

    (a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4) = 
      sbox3_r (a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4);
    (a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4) = 
      sbox3_r (a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4);

    return (a1 + a2 + a3 + a4, b1 + b2 + b3 + b4, c1 + c2 + c3 + c4);
  }

  proc sbox2_r(a1,a2,a3,a4,b1,b2,b3,b4) = {
    var c1,c2,c3,c4, d1,d2,d3,d4;
    (c1,c2,c3,c4) = mult_r(a1,a2,a3,a4,b1,b2,b3,b4);
    (d1,d2,d3,d4) = (a1 + c1,a2 + c2,a3 + c3,a4 + c4);
    return (c1,c2,c3,c4,d1,d2,d3,d4);
  }

  proc sbox4_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) = {
    var e1, e2, e3, e4, f1, f2, f3, f4;
    (e1,e2,e3,e4,f1,f2,f3,f4) = sbox2_r(c1,c2,c3,c4,d1,d2,d3,d4);
    (e1,e2,e3,e4) = (e1 + a1,e2 + a2,e3 + a3,e4 + a4);
    (f1,f2,f3,f4) = (f1 + b1,f2 + b2,f3 + b3,f4 + b4);
    return (c1,c2,c3,c4,d1,d2,d3,d4,e1,e2,e3,e4,f1,f2,f3,f4);
  }

  proc sbox4_1(a,b,c,d) = {
    var a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4, d1,d2,d3,d4;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = a + a1 + a2 + a3;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = b + b1 + b2 + b3;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = c + c1 + c2 + c3;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = d + d1 + d2 + d3;

    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    return (a1 + a2 + a3 + a4,b1 + b2 + b3 + b4,c1 + c2 + c3 + c4,d1 + d2 + d3 + d4);
  }

  proc sbox4_2(a,b,c,d) = {
    var a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4, d1,d2,d3,d4;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = a + a1 + a2 + a3;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = b + b1 + b2 + b3;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = c + c1 + c2 + c3;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = d + d1 + d2 + d3;

    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);

    return (a1 + a2 + a3 + a4,b1 + b2 + b3 + b4,c1 + c2 + c3 + c4,d1 + d2 + d3 + d4);
  }

  proc sbox4_3(a,b,c,d) = {
    var a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4, d1,d2,d3,d4;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = a + a1 + a2 + a3;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = b + b1 + b2 + b3;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = c + c1 + c2 + c3;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = d + d1 + d2 + d3;

    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);

    return (a1 + a2 + a3 + a4,b1 + b2 + b3 + b4,c1 + c2 + c3 + c4,d1 + d2 + d3 + d4);
  }

  proc sbox2r_r(a1,a2,a3,a4,b1,b2,b3,b4) = {
    var c1,c2,c3,c4, d1,d2,d3,d4;
    (c1,c2,c3,c4) = mult_r(a1,a2,a3,a4,b1,b2,b3,b4);
    (d1,d2,d3,d4) = (a1 + c1,a2 + c2,a3 + c3,a4 + c4);
    (d1,d2,d3,d4) = refresh_r(d1,d2,d3,d4);
    return (c1,c2,c3,c4,d1,d2,d3,d4);
  }

  proc sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) = {
    var e1, e2, e3, e4, f1, f2, f3, f4;
    (e1,e2,e3,e4,f1,f2,f3,f4) = sbox2_r(c1,c2,c3,c4,d1,d2,d3,d4);
    (e1,e2,e3,e4) = (e1 + a1,e2 + a2,e3 + a3,e4 + a4);
    (f1,f2,f3,f4) = (f1 + b1,f2 + b2,f3 + b3,f4 + b4);
    return (c1,c2,c3,c4,d1,d2,d3,d4,e1,e2,e3,e4,f1,f2,f3,f4);
  }

  proc sbox4r_3(a,b,c,d) = {
    var a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4, d1,d2,d3,d4;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = a + a1 + a2 + a3;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = b + b1 + b2 + b3;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = c + c1 + c2 + c3;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = d + d1 + d2 + d3;

    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);

    return (a1 + a2 + a3 + a4,b1 + b2 + b3 + b4,c1 + c2 + c3 + c4,d1 + d2 + d3 + d4);
  }

  proc sbox4r_4(a,b,c,d) = {
    var a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4, d1,d2,d3,d4;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = a + a1 + a2 + a3;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = b + b1 + b2 + b3;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = c + c1 + c2 + c3;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = d + d1 + d2 + d3;

    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);

    return (a1 + a2 + a3 + a4,b1 + b2 + b3 + b4,c1 + c2 + c3 + c4,d1 + d2 + d3 + d4);
  }

  proc sbox4r_5(a,b,c,d) = {
    var a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4, d1,d2,d3,d4;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = a + a1 + a2 + a3;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = b + b1 + b2 + b3;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = c + c1 + c2 + c3;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = d + d1 + d2 + d3;

    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);

    return (a1 + a2 + a3 + a4,b1 + b2 + b3 + b4,c1 + c2 + c3 + c4,d1 + d2 + d3 + d4);
  }

  proc sbox4r_6(a,b,c,d) = {
    var a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4, d1,d2,d3,d4;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = a + a1 + a2 + a3;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = b + b1 + b2 + b3;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = c + c1 + c2 + c3;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = d + d1 + d2 + d3;

    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);
    (a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4) =
      sbox4r_r(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4);

    return (a1 + a2 + a3 + a4,b1 + b2 + b3 + b4,c1 + c2 + c3 + c4,d1 + d2 + d3 + d4);
  }
}.
