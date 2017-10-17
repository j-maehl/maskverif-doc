sni  refresh (a 5) {
  shared r;

  r1 = $; r2 = $; r3 = $; r4 = $; r5 = $;

  a1 = a1 + r1;
  a2 = a2 + r2;
  a3 = a3 + r3;
  a4 = a4 + r4;
  a5 = a5 + r5;

  a1 = a1 + r5;
  a2 = a2 + r1;
  a3 = a3 + r2;
  a4 = a4 + r3;
  a5 = a5 + r4;

  return a;
}


  
   