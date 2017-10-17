sni pmul (a b 2) {
  var aux r;
  shared c;

  c1  = a1 * b1;
  c2  = a2 * b2;

  r   = $;
  c1  = c1 + r;
  aux = a1 * b2;
  c1  = c1 + aux;

  c2  = c2 + r;
  aux = a2 * b1;
  c2  = c2 + aux;

  return c;
}