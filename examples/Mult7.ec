require import Byte.

module Mult = {

  proc refresh_r (a1,a2,a3,a4,a5,a6,a7,a8) = {
    var r1, r2, r3, r4, r5, r6, r7, r8;
    var i1_1,i2_1,i3_1,i4_1,i5_1,i6_1,i7_1,i8_1,
        i1_2,i2_2,i3_2,i4_2,i5_2,i6_2,i7_2,i8_2,
        i1_3,i2_3,i3_3,i4_3,i5_3,i6_3,i7_3,i8_3,
        i1_4,i2_4,i3_4,i4_4,i5_4,i6_4,i7_4,i8_4,
        i1_5,i2_5,i3_5,i4_5,i5_5,i6_5,i7_5,i8_5: byte;

    r1 = $ distr; r2 = $ distr; r3 = $distr; r4 = $distr; r5 = $distr; r6 = $distr; r7 = $distr; r8 = $distr;


    (i1_2,i2_2,i3_2,i4_2,i5_2,i6_2,i7_2,i8_2) = (r1,r2,r3,r4,r5,r6,r7,r8);
    (i1_3,i2_3,i3_3,i4_3,i5_3,i6_3,i7_3,i8_3) = (r8,r1,r2,r3,r4,r5,r6,r7);
                     
    (i1_4,i2_4,i3_4,i4_4,i5_4,i6_4,i7_4,i8_4) = (a1+i1_2, a2+i2_2, a3+i3_2, a4+i4_2, a5+i5_2, a6+i6_2, a7 + i7_2, a8 + i8_2);
    (i1_5,i2_5,i3_5,i4_5,i5_5,i6_5,i7_5,i8_5) = (i1_4+i1_3, i2_4+i2_3, i3_4+i3_3, i4_4+i4_3,i5_4+i5_3, i6_4+i6_3, i7_4 + i7_3, i8_4 + i8_3);

    return (i1_5, i2_5, i3_5, i4_5, i5_5, i6_5, i7_5,i8_5);
  }

  proc refresh2_r(a1, a2, a3, a4, a5, a6,a7,a8: byte) = {
    var c1, c2, c3, c4, c5, c6,c7,c8;
    (c1,c2,c3,c4,c5,c6,c7,c8) = refresh_r(a1, a2, a3, a4, a5, a6,a7, a8);
    (c1,c2,c3,c4,c5,c6,c7,c8) = refresh_r(c1,c2,c3,c4,c5,c6,c7,c8);
    (c1,c2,c3,c4,c5,c6,c7,c8) = refresh_r(c1,c2,c3,c4,c5,c6,c7,c8);
    return (c1,c2,c3,c4,c5, c6,c7,c8);
  }
}.

(*hoare Correct a_ b_: Mult.mult: a = a_ /\ b = b_ ==> res = a_ * b_.
proof. by proc; inline *; auto=> />; progress; algebra. qed. *)

(*masking 6 Mult.mult Byte.ComRing.( * ) Byte.ComRing.(+).*)

masking 7 Mult.refresh2_r Byte.ComRing.( * ) Byte.ComRing.(+). 
