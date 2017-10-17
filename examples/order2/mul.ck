sni mul (a b 3) {
  var ji aux r;
  shared c;

  c1  = a1 * b1;
  c2  = a2 * b2;
  c3  = a3 * b3;

  r   = $;
  c1  = c1  + r;
  aux = a1  * b2;
  ji  = a2  * b1;
  aux = aux + r;
  aux = aux + ji;
  c2  = c2 + aux;

  r   = $;
  c1  = c1  + r;
  aux = a1  * b3;
  ji  = a3  * b1;
  aux = aux + r;
  aux = aux + ji;
  c3  = c3 + aux;

  r   = $;
  c2  = c2  + r;
  aux = a2  * b3;
  ji  = a3  * b2;
  aux = aux + r;
  aux = aux + ji;
  c3  = c3 + aux;

  return c;
}