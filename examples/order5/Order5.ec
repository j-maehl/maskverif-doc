require import Byte.

module Mult = {
  proc mult_r (a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6) = {
    var v1, v2, v3, v4, v5, v6;
    var v1_r0, v2_r0, v3_r0, v4_r0, v5_r0, v6_r0,
        v1_r1, v2_r1, v3_r1, v4_r1, v5_r1, v6_r1,
        v1_r2, v2_r2, v3_r2, v4_r2, v5_r2, v6_r2,
        v1_r3, v2_r3, v3_r3, v4_r3, v5_r3, v6_r3;
    var a1_r0, a2_r0, a3_r0, a4_r0, a5_r0, a6_r0,
        a1_r1, a2_r1, a3_r1, a4_r1, a5_r1, a6_r1,
        a1_r2, a2_r2, a3_r2, a4_r2, a5_r2, a6_r2,
        b1_r0, b2_r0, b3_r0, b4_r0, b5_r0, b6_r0,
        b1_r1, b2_r1, b3_r1, b4_r1, b5_r1, b6_r1,
        b1_r2, b2_r2, b3_r2, b4_r2, b5_r2, b6_r2,
        b1_r3, b2_r3, b3_r3, b4_r3, b5_r3, b6_r3;
    var ab001, ab002, ab003, ab004, ab005, ab006,
        ab011, ab012, ab013, ab014, ab015, ab016,
        ab101, ab102, ab103, ab104, ab105, ab106,
        ab021, ab022, ab023, ab024, ab025, ab026,
        ab201, ab202, ab203, ab204, ab205, ab206,
        ab031, ab032, ab033, ab034, ab035, ab036;
    var c1, c2, c3, c4, c5, c6;

    (* Blue *)
    (a1_r0,a2_r0,a3_r0,a4_r0,a5_r0,a6_r0) = (a1,a2,a3,a4,a5,a6);
    (b1_r0,b2_r0,b3_r0,b4_r0,b5_r0,b6_r0) = (b1,b2,b3,b4,b5,b6);
    (b1_r1,b2_r1,b3_r1,b4_r1,b5_r1,b6_r1) = (b2,b3,b4,b5,b6,b1);
    (a1_r1,a2_r1,a3_r1,a4_r1,a5_r1,a6_r1) = (a2,a3,a4,a5,a6,a1);
    (b1_r2,b2_r2,b3_r2,b4_r2,b5_r2,b6_r2) = (b3,b4,b5,b6,b1,b2);
    (a1_r2,a2_r2,a3_r2,a4_r2,a5_r2,a6_r2) = (a3,a4,a5,a6,a1,a2);
    (b1_r3,b2_r3,b3_r3,b4_r3,b5_r3,b6_r3) = (b4,b5,b6,b1,b2,b3);
                         
    (* Red *) 
    (ab001,ab002,ab003,ab004,ab005,ab006) = (a1_r0 * b1_r0,a2_r0 * b2_r0,a3_r0 * b3_r0,a4_r0 * b4_r0,a5_r0 * b5_r0,a6_r0 * b6_r0);
    (ab011,ab012,ab013,ab014,ab015,ab016) = (a1_r0 * b1_r1,a2_r0 * b2_r1,a3_r0 * b3_r1,a4_r0 * b4_r1,a5_r0 * b5_r1,a6_r0 * b6_r1);
    (ab101,ab102,ab103,ab104,ab105,ab106) = (a1_r1 * b1_r0,a2_r1 * b2_r0,a3_r1 * b3_r0,a4_r1 * b4_r0,a5_r1 * b5_r0,a6_r1 * b6_r0);
    (ab021,ab022,ab023,ab024,ab025,ab026) = (a1_r0 * b1_r2,a2_r0 * b2_r2,a3_r0 * b3_r2,a4_r0 * b4_r2,a5_r0 * b5_r2,a6_r0 * b6_r2);
    (ab201,ab202,ab203,ab204,ab205,ab206) = (a1_r2 * b1_r0,a2_r2 * b2_r0,a3_r2 * b3_r0,a4_r2 * b4_r0,a5_r2 * b5_r0,a6_r2 * b6_r0);
    (ab031,ab032,ab033,ab034,ab035,ab036) = (a1_r0 * b1_r3,a2_r0 * b2_r3,a3_r0 * b3_r3,a4_r0 * b4_r3,a5_r0 * b5_r3,a6_r0 * b6_r3);

    (* Green *)       
    v1 = $distr; v2 = $distr; v3 = $distr; v4 = $distr; v5 = $distr; v6 = $distr;
    (v1_r0,v2_r0,v3_r0,v4_r0,v5_r0,v6_r0) = (v1,v2,v3,v4,v5,v6);
    (v1_r1,v2_r1,v3_r1,v4_r1,v5_r1,v6_r1) = (v2,v3,v4,v5,v6,v1);
   

    (c1,c2,c3,c4,c5,c6) = (ab001 + v1_r0,ab002 + v2_r0,ab003 + v3_r0,ab004 + v4_r0,ab005 + v5_r0,ab006 + v6_r0);
    (c1,c2,c3,c4,c5,c6) = (c1    + ab011,c2    + ab012,c3    + ab013,c4    + ab014,c5    + ab015,c6    + ab016);
    (c1,c2,c3,c4,c5,c6) = (c1    + ab101,c2    + ab102,c3    + ab103,c4    + ab104,c5    + ab105,c6    + ab106);
    (c1,c2,c3,c4,c5,c6) = (c1    + v1_r1,c2    + v2_r1,c3    + v3_r1,c4    + v4_r1,c5    + v5_r1,c6    + v6_r1);
    (c1,c2,c3,c4,c5,c6) = (c1    + ab021,c2    + ab022,c3    + ab023,c4    + ab024,c5    + ab025,c6    + ab026);
    (c1,c2,c3,c4,c5,c6) = (c1    + ab201,c2    + ab202,c3    + ab203,c4    + ab204,c5    + ab205,c6    + ab206);
    v1 = $distr; v2 = $distr; v3 = $distr; v4 = $distr; v5 = $distr; v6 = $distr;
   (v1_r2,v2_r2,v3_r2,v4_r2,v5_r2,v6_r2) = (v1,v2,v3,v4,v5,v6);
   (v1_r3,v2_r3,v3_r3,v4_r3,v5_r3,v6_r3) = (v2,v3,v4,v5,v6,v1);

    (c1,c2,c3,c4,c5,c6) = (c1    + v1_r2,c2    + v2_r2,c3    + v3_r2,c4    + v4_r2,c5    + v5_r2,c6    + v6_r2);
    (c1,c2,c3,c4,c5,c6) = (c1    + ab031,c2    + ab032,c3    + ab033,c4    + ab034,c5    + ab035,c6    + ab036);
    (c1,c2,c3,c4,c5,c6) = (c1    + v1_r3,c2    + v2_r3,c3    + v3_r3,c4    + v4_r3,c5    + v5_r3,c6    + v6_r3);

    return (c1,c2,c3,c4,c5,c6);
  }

  proc mult (a,b) = {
    var a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6,c1,c2,c3,c4,c5,c6;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = $distr;
    a6 = a + a1 + a2 + a3 + a4 + a5;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = $distr;
    b5 = $distr;
    b6 = b + b1 + b2 + b3 + b4 + b5;

    (c1,c2,c3,c4,c5,c6) = mult_r(a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6);
    return (c1+c2+c3+c4+c5+c6);
  }

  proc refresh_r (a1,a2,a3,a4,a5,a6) = {
    var r1, r2, r3, r4, r5, r6;
    var i1_1,i2_1,i3_1,i4_1,i5_1,i6_1,
        i1_2,i2_2,i3_2,i4_2,i5_2,i6_2,
        i1_3,i2_3,i3_3,i4_3,i5_3,i6_3,
        i1_4,i2_4,i3_4,i4_4,i5_4,i6_4,
        i1_5,i2_5,i3_5,i4_5,i5_5,i6_5: byte;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; r5 = $distr; r6 = $distr;


    (i1_2,i2_2,i3_2,i4_2,i5_2,i6_2) = (r1,r2,r3,r4,r5,r6);
    (i1_3,i2_3,i3_3,i4_3,i5_3,i6_3) = (r6,r1,r2,r3,r4,r5);
                     
    (i1_4,i2_4,i3_4,i4_4,i5_4,i6_4) = (a1+i1_2, a2+i2_2, a3+i3_2, a4+i4_2, a5+i5_2, a6+i6_2);
    (i1_5,i2_5,i3_5,i4_5,i5_5,i6_5) = (i1_4+i1_3, i2_4+i2_3, i3_4+i3_3, i4_4+i4_3,i5_4+i5_3, i6_4+i6_3);

    return (i1_5, i2_5, i3_5, i4_5, i5_5, i6_5);
  }

  proc square (a) = {
   var a1, a2, a3, a4, a5, a6, b1, b2, b3, b4, b5, b6, c1, c2, c3, c4, c5, c6;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = $distr;
    a6 = a + a1 + a2 + a3 + a4 + a5;


    (b1,b2,b3,b4,b5,b6) = refresh_r(a1,a2,a3,a4,a5,a6);
    (a1,a2,a3,a4,a5,a6) = refresh_r(a1,a2,a3,a4,a5,a6);
    (c1,c2,c3,c4,c5,c6) = mult_r(a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6);
    return (c1 + c2 + c3 + c4 + c5 + c6);
  }

  proc or_r (a1,a2,a3,a4,a5,a6, b1,b2,b3,b4,b5,b6) = {
    var i1_1,i2_1,i3_1,i4_1,i5_1,i6_1, 
        i1_2,i2_2,i3_2,i4_2,i5_2,i6_2,
        i1_3,i2_3,i3_3,i4_3,i5_3,i6_3,
        i1_4,i2_4,i3_4,i4_4,i5_4,i6_4;
    (i1_1,i2_1,i3_1,i4_1,i5_1,i6_1) = (a1+oner,a2,a3,a4,a5,a6);
    (i1_2,i2_2,i3_2,i4_2,i5_2,i6_2) = (b1+oner,b2,b3,b4,b5,b6);
    (i1_3,i2_3,i3_3,i4_3,i5_3,i6_3) = mult_r(i1_1,i2_1,i3_1,i4_1,i5_1,i6_1, i1_2,i2_2,i3_2,i4_2,i5_2,i6_2);
    (i1_4,i2_4,i3_4,i4_4,i5_4,i6_4) = (i1_3+oner,i2_3,i3_3,i4_3,i5_3,i6_3);
    return (i1_4,i2_4,i3_4,i4_4,i5_4,i6_4);
  }
      
  proc sbox_r(a1,a2,a3,a4,a5,a6, b1,b2,b3,b4,b5,b6, c1,c2,c3,c4,c5,c6, d1,d2,d3,d4,d5,d6) = {
    var i1_1,i2_1,i3_1,i4_1,i5_1,i6_1,
        i1_2,i2_2,i3_2,i4_2,i5_2,i6_2,
        i1_3,i2_3,i3_3,i4_3,i5_3,i6_3,
        i1_4,i2_4,i3_4,i4_4,i5_4,i6_4,
        i1_5,i2_5,i3_5,i4_5,i5_5,i6_5,
        i1_6,i2_6,i3_6,i4_6,i5_6,i6_6,
        i1_7,i2_7,i3_7,i4_7,i5_7,i6_7,
        i1_8,i2_8,i3_8,i4_8,i5_8,i6_8;
    
    (i1_1,i2_1,i3_1,i4_1,i5_1,i6_1) = mult_r(a1,a2,a3,a4,a5,a6, b1,b2,b3,b4,b5,b6);
    (i1_2,i2_2,i3_2,i4_2,i5_2,i6_2) = (i1_1+c1, i2_1+c2, i3_1+c3, i4_1+c4, i5_1+c5, i6_1+c6);  (* out a *)
    (i1_3,i2_3,i3_3,i4_3,i5_3,i6_3) = or_r  (b1,b2,b3,b4,b5,b6, c1,c2,c3,c4,c5,c6);
    (i1_4,i2_4,i3_4,i4_4,i5_4,i6_4) = (i1_3+d1, i2_3+d2, i3_3+d3, i4_3+d4, i5_3+d5, i6_3+d6);  (* out c *)
    (i1_5,i2_5,i3_5,i4_5,i5_5,i6_5) = mult_r(i1_2,i2_2,i3_2,i4_2,i5_2,i6_2, d1,d2,d3,d4,d5,d6);
    (i1_6,i2_6,i3_6,i4_6,i5_6,i6_6) = (i1_5+a1,i2_5+a2,i3_5+a3,i4_5+a4,i5_5+a5,i6_5+a6);    (* out d *)  
    (i1_7,i2_7,i3_7,i4_7,i5_7,i6_7) = mult_r(i1_4,i2_4,i3_4,i4_4,i5_4,i6_4, a1,a2,a3,a4,a5,a6);
    (i1_8,i2_8,i3_8,i4_8,i5_8,i6_8) = (b1+i1_7,b2+i2_7,b3+i3_7,b4+i4_7,b5+i5_7,b6+i6_7);    (* out b *)

    return (i1_2,i2_2,i3_2,i4_2,i5_2,i6_2, i1_8,i2_8,i3_8,i4_8,i5_8,i6_8, 
            i1_4,i2_4,i3_4,i4_4,i5_4,i6_4, i1_6,i2_6,i3_6,i4_6,i5_6,i6_6);

  }

  proc sbox2(a,b,c,d) = {
    var a1,a2,a3,a4,a5,a6, b1,b2,b3,b4,b5,b6, c1,c2,c3,c4,c5,c6, d1,d2,d3,d4,d5,d6;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = $distr;
    a6 = a + a1 + a2 + a3 + a4 + a5;

    b1 = $distr;
    b2 = $distr;
    b3 = $distr;
    b4 = $distr;
    b5 = $distr;
    b6 = b + b1 + b2 + b3 + b4 + b5;

    c1 = $distr;
    c2 = $distr;
    c3 = $distr;
    c4 = $distr;
    c5 = $distr;
    c6 = c + c1 + c2 + c3 + c4 + c5;

    d1 = $distr;
    d2 = $distr;
    d3 = $distr;
    d4 = $distr;
    d5 = $distr;
    d6 = d + d1 + d2 + d3 + d4 + d5;


    (a1,a2,a3,a4,a5,a6, b1,b2,b3,b4,b5,b6, c1,c2,c3,c4,c5,c6, d1,d2,d3,d4,d5,d6) = 
      sbox_r(a1,a2,a3,a4,a5,a6, b1,b2,b3,b4,b5,b6, c1,c2,c3,c4,c5,c6, d1,d2,d3,d4,d5,d6);

    (a1,a2,a3,a4,a5,a6, b1,b2,b3,b4,b5,b6, c1,c2,c3,c4,c5,c6, d1,d2,d3,d4,d5,d6) =
      sbox_r(a1,a2,a3,a4,a5,a6, b1,b2,b3,b4,b5,b6, c1,c2,c3,c4,c5,c6, d1,d2,d3,d4,d5,d6);

    return (a1,a2,a3,a4,a5,a6, b1,b2,b3,b4,b5,b6, c1,c2,c3,c4,c5,c6, d1,d2,d3,d4,d5,d6);
  }

  proc rmult_r(a1, a2, a3, a4, a5,a6, b1, b2, b3, b4, b5,b6: byte) = {
    var c1, c2, c3, c4, c5, c6;
    (c1,c2,c3,c4,c5,c6) = mult_r(a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6);
    (c1,c2,c3,c4,c5,c6) = refresh_r(c1,c2,c3,c4,c5,c6);

    return (c1,c2,c3,c4,c5,c6);
  }

  proc multr_r(a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6: byte) = {
    var c1, c2, c3, c4, c5, c6;

    (b1,b2,b3,b4,b5,b6) = refresh_r(b1,b2,b3,b4,b5,b6);
    (c1,c2,c3,c4,c5,c6) = mult_r(a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6);

    return (c1,c2,c3,c4,c5,c6);
  }

  proc multrr_r(a1, a2, a3, a4, a5,a6, b1, b2, b3, b4, b5,b6: byte) = {
    var c1, c2, c3, c4, c5, c6;

    (a1,a2,a3,a4,a5,a6) = refresh_r(a1,a2,a3,a4,a5,a6);
    (b1,b2,b3,b4,b5,b6) = refresh_r(b1,b2,b3,b4,b5,b6);
    (c1,c2,c3,c4,c5,c6) = mult_r(a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6);

    return (c1,c2,c3,c4,c5,c6);
  }


  proc refresh2_r(a1, a2, a3, a4, a5, a6: byte) = {
    var c1, c2, c3, c4, c5, c6;
    (c1,c2,c3,c4,c5,c6) = refresh_r(a1, a2, a3, a4, a5, a6);
    (c1,c2,c3,c4,c5,c6) = refresh_r(c1,c2,c3,c4,c5,c6);

    return (c1,c2,c3,c4,c5, c6);
  }

  proc refresh (a) = {
   var a1, a2, a3, a4, a5, a6;
    a1 = $distr;
    a2 = $distr;
    a3 = $distr;
    a4 = $distr;
    a5 = $distr;
    a6 = a + a1 + a2 + a3 + a4 + a5;

    (a1,a2,a3,a4,a5,a6) = refresh_r(a1,a2,a3,a4,a5,a6);
    return (a1 + a2 + a3 + a4 + a5 + a6);
  }
}.
