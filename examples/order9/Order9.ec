require import Byte.

module Mult = {
  (* proc mult_r (a1,a2,a3,a4,a5,a6,a7,b1,b2,b3,b4,b5,b6,b7) = { *)
  (*   var v1, v2, v3, v4, v5, v6, v7; *)
  (*   var v1_r0, v2_r0, v3_r0, v4_r0, v5_r0, v6_r0,v7_r0, *)
  (*       v1_r1, v2_r1, v3_r1, v4_r1, v5_r1, v6_r1,v7_r1, *)
  (*       v1_r2, v2_r2, v3_r2, v4_r2, v5_r2, v6_r2,v7_r2, *)
  (*       v1_r3, v2_r3, v3_r3, v4_r3, v5_r3, v6_r3,v7_r3; *)
  (*   var a1_r0, a2_r0, a3_r0, a4_r0, a5_r0, a6_r0,a7_r0, *)
  (*       a1_r1, a2_r1, a3_r1, a4_r1, a5_r1, a6_r1,a7_r1, *)
  (*       a1_r2, a2_r2, a3_r2, a4_r2, a5_r2, a6_r2,a7_r2, *)
  (*       a1_r3, a2_r3, a3_r3, a4_r3, a5_r3, a6_r3,a7_r3, *)
  (*       b1_r0, b2_r0, b3_r0, b4_r0, b5_r0, b6_r0,b7_r0, *)
  (*       b1_r1, b2_r1, b3_r1, b4_r1, b5_r1, b6_r1,b7_r1, *)
  (*       b1_r2, b2_r2, b3_r2, b4_r2, b5_r2, b6_r2,b7_r2, *)
  (*       b1_r3, b2_r3, b3_r3, b4_r3, b5_r3, b6_r3,b7_r3; *)
  (*   var ab001, ab002, ab003, ab004, ab005, ab006,ab007, *)
  (*       ab011, ab012, ab013, ab014, ab015, ab016,ab017, *)
  (*       ab101, ab102, ab103, ab104, ab105, ab106,ab107, *)
  (*       ab021, ab022, ab023, ab024, ab025, ab026,ab027, *)
  (*       ab201, ab202, ab203, ab204, ab205, ab206,ab207, *)
  (*       ab031, ab032, ab033, ab034, ab035, ab036,ab037, *)
  (*       ab301, ab302, ab303, ab304, ab305, ab306,ab307; *)

  (*   var c1, c2, c3, c4, c5, c6, c7; *)

  (*   (* Blue *) *)
  (*   (a1_r0,a2_r0,a3_r0,a4_r0,a5_r0,a6_r0,a7_r0) = (a1,a2,a3,a4,a5,a6,a7); *)
  (*   (b1_r0,b2_r0,b3_r0,b4_r0,b5_r0,b6_r0,b7_r0) = (b1,b2,b3,b4,b5,b6,b7); *)
  (*   (b1_r1,b2_r1,b3_r1,b4_r1,b5_r1,b6_r1,b7_r1) = (b2,b3,b4,b5,b6,b7,b1); *)
  (*   (a1_r1,a2_r1,a3_r1,a4_r1,a5_r1,a6_r1,a7_r1) = (a2,a3,a4,a5,a6,a7,a1); *)
  (*   (b1_r2,b2_r2,b3_r2,b4_r2,b5_r2,b6_r2,b7_r2) = (b3,b4,b5,b6,b7,b1,b2); *)
  (*   (a1_r2,a2_r2,a3_r2,a4_r2,a5_r2,a6_r2,a7_r2) = (a3,a4,a5,a6,a7,a1,a2); *)
  (*   (b1_r3,b2_r3,b3_r3,b4_r3,b5_r3,b6_r3,b7_r3) = (b4,b5,b6,b7,b1,b2,b3); *)
  (*   (a1_r3,a2_r3,a3_r3,a4_r3,a5_r3,a6_r3,a7_r3) = (a4,a5,a6,a7,a1,a2,a3); *)

                         
  (*   (* Red *)  *)
  (*   (ab001,ab002,ab003,ab004,ab005,ab006,ab007) = (a1_r0 * b1_r0,a2_r0 * b2_r0,a3_r0 * b3_r0,a4_r0 * b4_r0,a5_r0 * b5_r0,a6_r0 * b6_r0, a7_r0 * b7_r0); *)
  (*   (ab011,ab012,ab013,ab014,ab015,ab016,ab017) = (a1_r0 * b1_r1,a2_r0 * b2_r1,a3_r0 * b3_r1,a4_r0 * b4_r1,a5_r0 * b5_r1,a6_r0 * b6_r1, a7_r0 * b7_r1); *)
  (*   (ab101,ab102,ab103,ab104,ab105,ab106,ab107) = (a1_r1 * b1_r0,a2_r1 * b2_r0,a3_r1 * b3_r0,a4_r1 * b4_r0,a5_r1 * b5_r0,a6_r1 * b6_r0, a7_r1 * b7_r0); *)
  (*   (ab021,ab022,ab023,ab024,ab025,ab026,ab027) = (a1_r0 * b1_r2,a2_r0 * b2_r2,a3_r0 * b3_r2,a4_r0 * b4_r2,a5_r0 * b5_r2,a6_r0 * b6_r2, a7_r0 * b7_r2); *)
  (*   (ab201,ab202,ab203,ab204,ab205,ab206,ab207) = (a1_r2 * b1_r0,a2_r2 * b2_r0,a3_r2 * b3_r0,a4_r2 * b4_r0,a5_r2 * b5_r0,a6_r2 * b6_r0, a7_r2 * b7_r0); *)
  (*   (ab031,ab032,ab033,ab034,ab035,ab036,ab037) = (a1_r0 * b1_r3,a2_r0 * b2_r3,a3_r0 * b3_r3,a4_r0 * b4_r3,a5_r0 * b5_r3,a6_r0 * b6_r3, a7_r0 * b7_r3); *)
  (*   (ab301,ab302,ab303,ab304,ab305,ab306,ab307) = (a1_r3 * b1_r0,a2_r3 * b2_r0,a3_r3 * b3_r0,a4_r3 * b4_r0,a5_r3 * b5_r0,a6_r3 * b6_r0, a7_r3 * b7_r0); *)

  (*   (* Green *)        *)
  (*   v1 = $distr; v2 = $distr; v3 = $distr; v4 = $distr; v5 = $distr; v6 = $distr; v7 = $distr; *)
  (*   (v1_r0,v2_r0,v3_r0,v4_r0,v5_r0,v6_r0,v7_r0) = (v1,v2,v3,v4,v5,v6,v7); *)
  (*   (v1_r1,v2_r1,v3_r1,v4_r1,v5_r1,v6_r1,v7_r1) = (v2,v3,v4,v5,v6,v7,v1); *)
   

  (*   (c1,c2,c3,c4,c5,c6,c7) = (ab001 + v1_r0,ab002 + v2_r0,ab003 + v3_r0,ab004 + v4_r0,ab005 + v5_r0,ab006 + v6_r0, ab007 + v7_r0); *)
  (*   (c1,c2,c3,c4,c5,c6,c7) = (c1    + ab011,c2    + ab012,c3    + ab013,c4    + ab014,c5    + ab015,c6    + ab016, c7 + ab017); *)
  (*   (c1,c2,c3,c4,c5,c6,c7) = (c1    + ab101,c2    + ab102,c3    + ab103,c4    + ab104,c5    + ab105,c6    + ab106, c7 + ab107); *)
  (*   (c1,c2,c3,c4,c5,c6,c7) = (c1    + v1_r1,c2    + v2_r1,c3    + v3_r1,c4    + v4_r1,c5    + v5_r1,c6    + v6_r1, c7 + v7_r1); *)
  (*   (c1,c2,c3,c4,c5,c6,c7) = (c1    + ab021,c2    + ab022,c3    + ab023,c4    + ab024,c5    + ab025,c6    + ab026, c7 + ab027); *)
  (*   (c1,c2,c3,c4,c5,c6,c7) = (c1    + ab201,c2    + ab202,c3    + ab203,c4    + ab204,c5    + ab205,c6    + ab206, c7 + ab207); *)

  (*   v1 = $distr; v2 = $distr; v3 = $distr; v4 = $distr; v5 = $distr; v6 = $distr; v7 = $distr; *)
  (*   (v1_r2,v2_r2,v3_r2,v4_r2,v5_r2,v6_r2,v7_r2) = (v1,v2,v3,v4,v5,v6,v7); *)
  (*   (v1_r3,v2_r3,v3_r3,v4_r3,v5_r3,v6_r3,v7_r3 ) = (v2,v3,v4,v5,v6,v7,v1); *)

    

  (*   (c1,c2,c3,c4,c5,c6,c7) = (c1 + v1_r2, c2 + v2_r2, c3 + v3_r2, c4 + v4_r2, c5 + v5_r2, c6 + v6_r2, c7 + v7_r2); *)
  (*   (c1,c2,c3,c4,c5,c6,c7) = (c1 + ab031, c2 + ab032, c3 + ab033, c4 + ab034, c5 + ab035, c6 + ab036, c7 + ab037); *)
  (*   (c1,c2,c3,c4,c5,c6,c7) = (c1 + ab301, c2 + ab302, c3 + ab303, c4 + ab304, c5 + ab305, c6 + ab306, c7 + ab307); *)
  (*   (c1,c2,c3,c4,c5,c6,c7) = (c1 + v1_r3, c2 + v2_r3, c3 + v3_r3, c4 + v4_r3, c5 + v5_r3, c6 + v6_r3, c7 + v7_r3); *)

  (*   return (c1,c2,c3,c4,c5,c6,c7); *)
  (* } *)

  (* proc mult (a,b) = { *)
  (*   var a1,a2,a3,a4,a5,a6,a7,b1,b2,b3,b4,b5,b6,b7,c1,c2,c3,c4,c5,c6,c7; *)
  (*   a1 = $distr; *)
  (*   a2 = $distr; *)
  (*   a3 = $distr; *)
  (*   a4 = $distr; *)
  (*   a5 = $distr; *)
  (*   a6 = $distr; *)
  (*   a7 = a + a1 + a2 + a3 + a4 + a5 + a6; *)

  (*   b1 = $distr; *)
  (*   b2 = $distr; *)
  (*   b3 = $distr; *)
  (*   b4 = $distr; *)
  (*   b5 = $distr; *)
  (*   b6 = $distr; *)
  (*   b7 = b + b1 + b2 + b3 + b4 + b5 + b6; *)

  (*   (c1,c2,c3,c4,c5,c6,c7) = mult_r(a1,a2,a3,a4,a5,a6,a7,b1,b2,b3,b4,b5,b6,b7); *)
  (*   return (c1+c2+c3+c4+c5+c6+c7); *)
  (* } *)

  (* proc mult2 (a,b) = { *)
  (*   var a1,a2,a3,a4,a5,a6,a7,b1,b2,b3,b4,b5,b6,b7,c1,c2,c3,c4,c5,c6,c7; *)
  (*   a1 = $distr; *)
  (*   a2 = $distr; *)
  (*   a3 = $distr; *)
  (*   a4 = $distr; *)
  (*   a5 = $distr; *)
  (*   a6 = $distr; *)
  (*   a7 = a + a1 + a2 + a3 + a4 + a5 + a6; *)

  (*   b1 = $distr; *)
  (*   b2 = $distr; *)
  (*   b3 = $distr; *)
  (*   b4 = $distr; *)
  (*   b5 = $distr; *)
  (*   b6 = $distr; *)
  (*   b7 = b + b1 + b2 + b3 + b4 + b5 + b6; *)

  (*   (c1,c2,c3,c4,c5,c6,c7) = mult_r(a1,a2,a3,a4,a5,a6,a7,b1,b2,b3,b4,b5,b6,b7); *)
  (*   (c1,c2,c3,c4,c5,c6,c7) = mult_r(a1,a2,a3,a4,a5,a6,a7,c1,c2,c3,c4,c5,c6,c7); *)
  (*   return (c1+c2+c3+c4+c5+c6+c7); *)
  (* } *)

  proc refresh_r (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = {
    var r1, r2, r3, r4, r5, r6, r7, r8, r9, r10;
    var i1_1,i2_1,i3_1,i4_1,i5_1,i6_1,i7_1,i8_1,i9_1,i10_1,
        i1_2,i2_2,i3_2,i4_2,i5_2,i6_2,i7_2,i8_2,i9_2,i10_2,
        i1_3,i2_3,i3_3,i4_3,i5_3,i6_3,i7_3,i8_3,i9_3,i10_3,
        i1_4,i2_4,i3_4,i4_4,i5_4,i6_4,i7_4,i8_4,i9_4,i10_4,
        i1_5,i2_5,i3_5,i4_5,i5_5,i6_5,i7_5,i8_5,i9_5,i10_5: byte;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr; r9 = $distr; r10 = $distr;


    (i1_2,i2_2,i3_2,i4_2,i5_2,i6_2,i7_2,i8_2,i9_2,i10_2) = (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10);
    (i1_3,i2_3,i3_3,i4_3,i5_3,i6_3,i7_3,i8_3,i9_3,i10_3) = (r10,r1,r2,r3,r4,r5,r6,r7,r8,r9);
                     
    (i1_4,i2_4,i3_4,i4_4,i5_4,i6_4,i7_4,i8_4,i9_4,i10_4) = (a1+i1_2, a2+i2_2, a3+i3_2, a4+i4_2, a5+i5_2, a6+i6_2, a7 + i7_2, a8 + i8_2, a9 + i9_2, a10 + i10_2);
    (i1_5,i2_5,i3_5,i4_5,i5_5,i6_5,i7_5,i8_5,i9_5,i10_5) = (i1_4+i1_3, i2_4+i2_3, i3_4+i3_3, i4_4+i4_3,i5_4+i5_3, i6_4+i6_3, i7_4 + i7_3, i8_4 + i8_3, i9_4 + i9_3, i10_4 + i10_3);

    return (i1_5, i2_5, i3_5, i4_5, i5_5, i6_5, i7_5, i8_5, i9_5, i10_5);
  }

  proc refresh2_r(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10: byte) = {
    var c1, c2, c3, c4, c5, c6, c7, c8, c9, c10;
    (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) = refresh_r(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
    (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) = refresh_r(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10);

    return (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10);
  }

  proc refresh3_r(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10: byte) = {
    var c1, c2, c3, c4, c5, c6, c7, c8, c9, c10;
    (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) = refresh_r(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
    (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) = refresh_r(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10);
    (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) = refresh_r(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10);

    return (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10);
  }
}.
