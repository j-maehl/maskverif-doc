require import Byte.

module Mult = {

  proc refresh_r (a1,a2,a3,a4,a5) = {
    var r1, r2, r3, r4, r5;
    var i1_1,i2_1,i3_1,i4_1,i5_1,
        i1_2,i2_2,i3_2,i4_2,i5_2,
        i1_3,i2_3,i3_3,i4_3,i5_3,
        i1_4,i2_4,i3_4,i4_4,i5_4,
        i1_5,i2_5,i3_5,i4_5,i5_5: byte;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; r5 = $distr;


    (i1_2,i2_2,i3_2,i4_2,i5_2) = (r1,r2,r3,r4,r5);
    (i1_3,i2_3,i3_3,i4_3,i5_3) = (r5,r1,r2,r3,r4);
                     
    (i1_4,i2_4,i3_4,i4_4,i5_4) = (a1+i1_2, a2+i2_2, a3+i3_2, a4+i4_2, a5+i5_2);
    (i1_5,i2_5,i3_5,i4_5,i5_5) = (i1_4+i1_3, i2_4+i2_3, i3_4+i3_3, i4_4+i4_3, 
                                    i5_4+i5_3);

    return (i1_5, i2_5, i3_5, i4_5, i5_5);
  }

  proc refresh (a:byte): byte = {
    var a1, a2, a3, a4, a5;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = a + a1 + a2 + a3 + a4;

    (a1,a2,a3,a4,a5) = refresh_r(a1,a2,a3,a4,a5);
    return (a1 + a2 + a3 + a4 + a5);
  }

  proc refresh2_r(a1, a2, a3, a4, a5: byte) = {
    var c1, c2, c3, c4, c5;
    (c1,c2,c3,c4,c5) = refresh_r(a1,a2,a3,a4,a5);
    (c1,c2,c3,c4,c5) = refresh_r(c1,c2,c3,c4,c5);

    return (c1,c2,c3,c4,c5);
  }

  proc mult_r(a1, a2, a3, a4, a5, b1, b2, b3, b4, b5: byte) = {

    var r1, r2, r3, r4, r5;
    var i1_1,i2_1,i3_1,i4_1,i5_1,
        i1_2,i2_2,i3_2,i4_2,i5_2,
        i1_3,i2_3,i3_3,i4_3,i5_3,
        i1_4,i2_4,i3_4,i4_4,i5_4,
        i1_5,i2_5,i3_5,i4_5,i5_5,
        i1_6,i2_6,i3_6,i4_6,i5_6,
        i1_7,i2_7,i3_7,i4_7,i5_7,
        i1_8,i2_8,i3_8,i4_8,i5_8, 
        i1_9,i2_9,i3_9,i4_9,i5_9,
        i1_10,i2_10,i3_10,i4_10,i5_10,
        i1_11,i2_11,i3_11,i4_11,i5_11,
        i1_12,i2_12,i3_12,i4_12,i5_12,
        i1_13,i2_13,i3_13,i4_13,i5_13,
        i1_14,i2_14,i3_14,i4_14,i5_14,
        i1_15,i2_15,i3_15,i4_15,i5_15,
        i1_16,i2_16,i3_16,i4_16,i5_16,
        i1_17,i2_17,i3_17,i4_17,i5_17,
        i1_18,i2_18,i3_18,i4_18,i5_18,
        i1_19,i2_19,i3_19,i4_19,i5_19,
        i1_20,i2_20,i3_20,i4_20,i5_20 : byte;

    (* Blue *)
    (i1_1,i2_1,i3_1,i4_1,i5_1) = (a1,a2,a3,a4,a5);
    (i1_2,i2_2,i3_2,i4_2,i5_2) = (b1,b2,b3,b4,b5);
    (i1_3,i2_3,i3_3,i4_3,i5_3) = (b2,b3,b4,b5,b1);
    (i1_4,i2_4,i3_4,i4_4,i5_4) = (a2,a3,a4,a5,a1);
    (i1_5,i2_5,i3_5,i4_5,i5_5) = (b3,b4,b5,b1,b2);
    (i1_6,i2_6,i3_6,i4_6,i5_6) = (a3,a4,a5,a1,a2);
                         
    (* Red *) 
    (i1_7,i2_7,i3_7,i4_7,i5_7)      = (i1_1 * i1_2, i2_1 * i2_2, i3_1 * i3_2, i4_1 * i4_2, i5_1 * i5_2);

    (i1_8,i2_8,i3_8,i4_8,i5_8)      = (i1_1 * i1_3, i2_1 * i2_3, i3_1 * i3_3, i4_1 * i4_3, i5_1 * i5_3);
    (i1_9,i2_9,i3_9,i4_9,i5_9)      = (i1_4 * i1_2, i2_4 * i2_2, i3_4 * i3_2, i4_4 * i4_2, i5_4 * i5_2);

    (i1_10,i2_10,i3_10,i4_10,i5_10) = (i1_1 * i1_5, i2_1 * i2_5, i3_1 * i3_5, i4_1 * i4_5, i5_1 * i5_5);
    (i1_11,i2_11,i3_11,i4_11,i5_11) = (i1_6 * i1_2, i2_6 * i2_2, i3_6 * i3_2, i4_6 * i4_2, i5_6 * i5_2);

    (* Green *)       
    r1 = $distr; r2 = $distr; r3 = $distr; r4 = $distr; r5 = $distr;
    (i1_12,i2_12,i3_12,i4_12,i5_12) = (r1,r2,r3,r4,r5);
    (i1_13,i2_13,i3_13,i4_13,i5_13) = (r2,r3,r4,r5,r1);

    (i1_14,i2_14,i3_14,i4_14,i5_14) = (i1_7  + i1_12, i2_7  + i2_12, i3_7  + i3_12, i4_7  + i4_12, i5_7  + i5_12);
    (i1_15,i2_15,i3_15,i4_15,i5_15) = (i1_14 + i1_8 , i2_14 + i2_8 , i3_14 + i3_8 , i4_14 + i4_8 , i5_14 + i5_8 );
    (i1_16,i2_16,i3_16,i4_16,i5_16) = (i1_15 + i1_9 , i2_15 + i2_9 , i3_15 + i3_9 , i4_15 + i4_9 , i5_15 + i5_9 );
    (i1_17,i2_17,i3_17,i4_17,i5_17) = (i1_16 + i1_13, i2_16 + i2_13, i3_16 + i3_13, i4_16 + i4_13, i5_16 + i5_13); 
    (i1_18,i2_18,i3_18,i4_18,i5_18) = (i1_17 + i1_10, i2_17 + i2_10, i3_17 + i3_10, i4_17 + i4_10, i5_17 + i5_10);
    (i1_19,i2_19,i3_19,i4_19,i5_19) = (i1_18 + i1_11, i2_18 + i2_11, i3_18 + i3_11, i4_18 + i4_11, i5_18 + i5_11);

    return (i1_19,i2_19,i3_19,i4_19,i5_19);
  }

  proc mult(a b: byte): byte = {
    var a1, a2, a3, a4, a5, b1, b2, b3, b4, b5, c1,c2,c3,c4,c5;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = a + a1 + a2 + a3 + a4;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = $distr;
    b5 = b + b1 + b2 + b3 + b4;

    (c1,c2,c3,c4,c5) = mult_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5);
    return (c1+c2+c3+c4+c5);
  }

  proc rmult_r(a1, a2, a3, a4, a5, b1, b2, b3, b4, b5: byte) = {
    var c1, c2, c3, c4, c5;
    (c1,c2,c3,c4,c5) = mult_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5);
    (c1,c2,c3,c4,c5) = refresh_r(c1,c2,c3,c4,c5);
    return (c1,c2,c3,c4,c5);
  }

  proc multrr_r(a1, a2, a3, a4, a5, b1, b2, b3, b4, b5: byte) = {
    var c1, c2, c3, c4, c5;

    (a1,a2,a3,a4,a5) = refresh_r(a1,a2,a3,a4,a5);
    (b1,b2,b3,b4,b5) = refresh_r(b1,b2,b3,b4,b5);
    (c1,c2,c3,c4,c5) = mult_r(a1, a2, a3, a4, a5, b1, b2, b3, b4, b5);

    return (c1,c2,c3,c4,c5);
  }

  proc multr_r(a1, a2, a3, a4, a5, b1, b2, b3, b4, b5: byte) = {
    var c1, c2, c3, c4, c5;

    (b1,b2,b3,b4,b5) = refresh_r(b1,b2,b3,b4,b5);
    (c1,c2,c3,c4,c5) = mult_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5);

    return (c1,c2,c3,c4,c5);
  }

  proc sbox3_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5) = {
    var i1_1,i2_1,i3_1,i4_1,i5_1,
        i1_2,i2_2,i3_2,i4_2,i5_2,
        i1_3,i2_3,i3_3,i4_3,i5_3,
        i1_4,i2_4,i3_4,i4_4,i5_4,
        i1_5,i2_5,i3_5,i4_5,i5_5,
        i1_6,i2_6,i3_6,i4_6,i5_6;
    
    (i1_1,i2_1,i3_1,i4_1,i5_1) = mult_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5);
    (i1_2,i2_2,i3_2,i4_2,i5_2) = (i1_1+c1, i2_1+c2, i3_1+c3, i4_1+c4, i5_1+c5);  (* out a *)
    (i1_3,i2_3,i3_3,i4_3,i5_3) = mult_r(b1,b2,b3,b4,b5,c1,c2,c3,c4,c5);
    (i1_4,i2_4,i3_4,i4_4,i5_4) = (i1_3+a1, i2_3+a2, i3_3+a3, i4_3+a4, i5_3+a5);  (* out b *)
    (i1_5,i2_5,i3_5,i4_5,i5_5) = mult_r(c1,c2,c3,c4,c5,a1,a2,a3,a4,a5);
    (i1_6,i2_6,i3_6,i4_6,i5_6) = (i1_5+b1, i2_5+b2, i3_5+b3, i4_5+b4, i5_5+b5);  (* out c *)  

    return (i1_2,i2_2,i3_2,i4_2,i5_2,i1_4,i2_4,i3_4,i4_4,i5_4,i1_6,i2_6,i3_6,i4_6,i5_6);
  }

  proc sbox3_1(a,b,c) = {
    var a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = a + a1 + a2 + a3 + a4;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = $distr;
    b5 = b + b1 + b2 + b3 + b4;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = $distr;
    c5 = c + c1 + c2 + c3 + c4;

    (a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5) = 
      sbox3_r (a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5);

    return (a1 + a2 + a3 + a4 + a5, b1 + b2 + b3 + b4 + b5, c1 + c2 + c3 + c4 + c5);
  }

  proc sbox3_2(a,b,c) = {
    var a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = a + a1 + a2 + a3 + a4;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = $distr;
    b5 = b + b1 + b2 + b3 + b4;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = $distr;
    c5 = c + c1 + c2 + c3 + c4;

    (a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5) = 
      sbox3_r (a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5);
    (a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5) = 
      sbox3_r (a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5);

    return (a1 + a2 + a3 + a4 + a5, b1 + b2 + b3 + b4 + b5, c1 + c2 + c3 + c4 + c5);
  }

  proc sbox2_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5) = {
    var c1,c2,c3,c4,c5, d1,d2,d3,d4,d5;
    (c1,c2,c3,c4,c5) = mult_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5);
    (d1,d2,d3,d4,d5) = (a1 + c1,a2 + c2,a3 + c3,a4 + c4,a5 + c5);
    return (c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
  }

  proc sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) = {
    var e1, e2, e3, e4, e5, f1, f2, f3, f4, f5;
    (e1,e2,e3,e4,e5,f1,f2,f3,f4,f5) = sbox2_r(c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (e1,e2,e3,e4,e5) = (e1 + a1,e2 + a2,e3 + a3,e4 + a4,e5 + a5);
    (f1,f2,f3,f4,f5) = (f1 + b1,f2 + b2,f3 + b3,f4 + b4,f5 + b5);
    return (c1,c2,c3,c4,c5,d1,d2,d3,d4,d5,e1,e2,e3,e4,e5,f1,f2,f3,f4,f5);
  }

  proc sbox4_1(a,b,c,d) = {
    var a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5, d1,d2,d3,d4,d5;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = a + a1 + a2 + a3 + a4;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = $distr;
    b5 = b + b1 + b2 + b3 + b4;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = $distr;
    c5 = c + c1 + c2 + c3 + c4;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = $distr;
    d5 = d + d1 + d2 + d3 + d4;

    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    return (a1 + a2 + a3 + a4 + a5,b1 + b2 + b3 + b4 + b5,c1 + c2 + c3 + c4 + c5,d1 + d2 + d3 + d4 + d5);
  }

  proc sbox4_2(a,b,c,d) = {
    var a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5, d1,d2,d3,d4,d5;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = a + a1 + a2 + a3 + a4;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = $distr;
    b5 = b + b1 + b2 + b3 + b4;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = $distr;
    c5 = c + c1 + c2 + c3 + c4;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = $distr;
    d5 = d + d1 + d2 + d3 + d4;

    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);

    return (a1 + a2 + a3 + a4 + a5,b1 + b2 + b3 + b4 + b5,c1 + c2 + c3 + c4 + c5,d1 + d2 + d3 + d4 + d5);
  }

  proc sbox4_3(a,b,c,d) = {
    var a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5, d1,d2,d3,d4,d5;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = a + a1 + a2 + a3 + a4;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = $distr;
    b5 = b + b1 + b2 + b3 + b4;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = $distr;
    c5 = c + c1 + c2 + c3 + c4;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = $distr;
    d5 = d + d1 + d2 + d3 + d4;

    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);

    return (a1 + a2 + a3 + a4 + a5,b1 + b2 + b3 + b4 + b5,c1 + c2 + c3 + c4 + c5,d1 + d2 + d3 + d4 + d5);
  }

  proc sbox2r_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5) = {
    var c1,c2,c3,c4,c5, d1,d2,d3,d4,d5;
    (c1,c2,c3,c4,c5) = mult_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5);
    (d1,d2,d3,d4,d5) = (a1 + c1,a2 + c2,a3 + c3,a4 + c4,a5 + c5);
    (d1,d2,d3,d4,d5) = refresh_r(d1,d2,d3,d4,d5);
    return (c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
  }

  proc sbox4r_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) = {
    var e1, e2, e3, e4, e5, f1, f2, f3, f4, f5;
    (e1,e2,e3,e4,e5,f1,f2,f3,f4,f5) = sbox2r_r(c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (e1,e2,e3,e4,e5) = (e1 + a1,e2 + a2,e3 + a3,e4 + a4,e5 + a5);
    (f1,f2,f3,f4,f5) = (f1 + b1,f2 + b2,f3 + b3,f4 + b4,f5 + b5);
    return (c1,c2,c3,c4,c5,d1,d2,d3,d4,d5,e1,e2,e3,e4,e5,f1,f2,f3,f4,f5);
  }

  proc sbox4r_3(a,b,c,d) = {
    var a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5, d1,d2,d3,d4,d5;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = a + a1 + a2 + a3 + a4;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = $distr;
    b5 = b + b1 + b2 + b3 + b4;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = $distr;
    c5 = c + c1 + c2 + c3 + c4;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = $distr;
    d5 = d + d1 + d2 + d3 + d4;

    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);

    return (a1 + a2 + a3 + a4 + a5,b1 + b2 + b3 + b4 + b5,c1 + c2 + c3 + c4 + c5,d1 + d2 + d3 + d4 + d5);
  }

  proc sbox4r_4(a,b,c,d) = {
    var a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5, d1,d2,d3,d4,d5;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = a + a1 + a2 + a3 + a4;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = $distr;
    b5 = b + b1 + b2 + b3 + b4;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = $distr;
    c5 = c + c1 + c2 + c3 + c4;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = $distr;
    d5 = d + d1 + d2 + d3 + d4;

    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);

    return (a1 + a2 + a3 + a4 + a5,b1 + b2 + b3 + b4 + b5,c1 + c2 + c3 + c4 + c5,d1 + d2 + d3 + d4 + d5);
  }

  proc sbox4r_5(a,b,c,d) = {
    var a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5, d1,d2,d3,d4,d5;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = a + a1 + a2 + a3 + a4;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = $distr;
    b5 = b + b1 + b2 + b3 + b4;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = $distr;
    c5 = c + c1 + c2 + c3 + c4;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = $distr;
    d5 = d + d1 + d2 + d3 + d4;

    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);

    return (a1 + a2 + a3 + a4 + a5,b1 + b2 + b3 + b4 + b5,c1 + c2 + c3 + c4 + c5,d1 + d2 + d3 + d4 + d5);
  }
 
  proc square_refresh (a1, a2, a3, a4, a5: byte) = {
    var c1, c2, c3, c4, c5;
    (c1,c2,c3,c4,c5) = refresh_r(a1, a2, a3, a4, a5);
    (c1,c2,c3,c4,c5) = mult_r(a1, a2, a3, a4, a5, c1,c2,c3,c4,c5);
    return (c1,c2,c3,c4,c5);
  }

  proc sbox4r_6(a,b,c,d) = {
    var a1,a2,a3,a4,a5, b1,b2,b3,b4,b5, c1,c2,c3,c4,c5, d1,d2,d3,d4,d5;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = a + a1 + a2 + a3 + a4;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = $distr;
    b5 = b + b1 + b2 + b3 + b4;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = $distr;
    c5 = c + c1 + c2 + c3 + c4;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = $distr;
    d5 = d + d1 + d2 + d3 + d4;

    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);
    (a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5) =
      sbox4_r(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5);

    return (a1 + a2 + a3 + a4 + a5,b1 + b2 + b3 + b4 + b5,c1 + c2 + c3 + c4 + c5,d1 + d2 + d3 + d4 + d5);
  }
}.
