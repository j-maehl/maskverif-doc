sni mul (a b 2) {
  var ji aux r;
  shared c;

  c1  = a1  * b1;
  c2  = a2  * b2;

  r   = $;
  c1  = c1  + r;
  aux = a1  * b2;
  ji  = a2  * b1;
  aux = aux + r;
  aux = aux + ji;
  c2  = c2 + aux;
  return c;
}