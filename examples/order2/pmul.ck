sni pmul(a b 3) {
  shared c r;
  var ij;

  r1 = $; r2 = $; r3 = $;

  c1 = a1 * b1;
  c2 = a2 * b2;
  c3 = a3 * b3;

  c1 = c1 + r1;
  c2 = c2 + r2;
  c3 = c3 + r3;

  ij = a1 * b3;
  c1 = c1 + ij;
  ij = a2 * b1;
  c2 = c2 + ij;
  ij = a3 * b2;
  c3 = c3 + ij;

  ij = a3 * b1;
  c1 = c1 + ij;
  ij = a1 * b2;
  c2 = c2 + ij;
  ij = a2 * b3;
  c3 = c3 + ij;

  c1 = c1 + r3;
  c2 = c2 + r1;
  c3 = c3 + r2;
  
  return c;

}