require import Byte.
 
op l1 : byte = oner.
op l2 : byte = oner + oner + oner + oner + oner.


module M = {
proc mult_r (a1,a2,b1,b2) = {
  var r, u12, u21 : byte;
  var c1, c2: byte;
  var t11, t12, t21, t22:byte;
  var du : byte;
  var u11', u12', u21', u22':byte;
  var v11, v12, v21, v22:byte;

  c1  = b1; (* * l1 *)
  t11 = a1 * c1; (* a1*b1*l1; *)
  t21 = a2 * c1; (* a2*b1*l1; *)
  c2  = b2 * l2;
  t12 = a1 * c2; (* a1*b2*l2; *)
  t22 = a2 * c2; (* a2*b2*l2; *)
    
  r = $distr;
  u12 = r;
  u21 = r;

  u12 = u12;
  u21 = u21 * invr l2;

  v11 = t11;
  v12 = t12 + u12'; 
  v21 = t21 + u21'; 
  v22 = t22;
  c1  = v11 + v12;
  c2  = v21 + v22;

  return (c1,c2);
  
}

proc mult (a,b) = {
  var a1,a2,b1,b2,c1,c2;
    a1 = $distr;
    a2 = a + a1;

    b1 = $distr;
    b2 = b + b1;

    (c1,c2) = mult_r(a1,a2,b1,b2);
    return (c1+c2);
  }

}.

masking ibmoment 1 8 Byte M.mult.



(a11, a21)   indep si ok 
else E (a11 . a21)




   