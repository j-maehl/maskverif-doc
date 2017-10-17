require import Byte.
module M = {
  proc refresh(a0 a1 a2 a3 a4 a5) = {
    var rA,rB,rC,r2,r3,r4,r5:byte;
    var c0,c1,c2,c3,c4,c5:byte;
    rA = $distr; rB = $distr; rC = $distr;
    r2 = $distr; r3 = $distr; r4 = $distr; r5 = $distr;
    (* c0 = rA+a0+rC+r2+rB+r4; *)
    c0 = rA + a0;
    c0 = c0 + rC;
    c0 = c0 + r2;
    c0 = c0 + rB;
    c0 = c0 + r4;
    (* c1 = rB+a1+rA+r3+rC+r5 *)
    c1 = rB + a1;
    c1 = c1 + rA;
    c1 = c1 + r3;
    c1 = c1 + rC;
    c1 = c1 + r5;
    c2 = a2+r2;
    c3 = a3+r3;
    c4 = a4+r4;
    c5 = a5+r5;
    return (c0,c1,c2,c3,c4,c5);
  }
}.

masking sni 5 M.refresh Byte.ComRing.(+).

module M1 = {
  proc refresh(a0 a1 a2) = {
    var r0,r1,r2:byte;
    var c0,c1,c2:byte;
    r0 = $distr; r1 = $distr; r2 = $distr;
    c0 = r1+a0;
    c0 = c0+r0;
    c0 = c0+a1;
    c0 = c0+r2;
    c1 = r1+r2;
    c2 = a2+r0;
    return (c0,c1,c2);
}}.

masking sni 2 M1.refresh Byte.ComRing.(+).
