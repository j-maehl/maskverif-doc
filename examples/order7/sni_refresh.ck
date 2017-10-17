sni 3 refresh (a 8) {
  shared r;

  r1 = $; r2 = $; r3 = $; r4 = $; r5 = $; r6 = $; r7 = $; r8 = $;

  a1 = a1 + r1;
  a2 = a2 + r2;
  a3 = a3 + r3;
  a4 = a4 + r4;
  a5 = a5 + r5;
  a6 = a6 + r6;
  a7 = a7 + r7;
  a8 = a8 + r8;

  a1 = a1 + r8;
  a2 = a2 + r1;
  a3 = a3 + r2;
  a4 = a4 + r3;
  a5 = a5 + r4;
  a6 = a6 + r5;
  a7 = a7 + r6;
  a8 = a8 + r7;

  return a;
}


  
   