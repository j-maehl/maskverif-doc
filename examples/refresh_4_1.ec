require import Byte.

module M = {
  proc refresh(b0, b1, b2, b3, b4) = {
    var a0, a1, a2, a3, a4;
    var r0, r1, r2, r3, r4;
    r0 <$ distr;
    r1 <$ distr;
    r2 <$ distr;
    r3 <$ distr;
    r4 <$ distr;
    
    
    
    a0 = r0 + r4;
    a1 = r1 + r0;
    a2 = r2 + r1;
    a3 = r3 + r2;
    a4 = r4 + r3;
    
    a0 = a0 + b0;
    a1 = a1 + b1;
    a2 = a2 + b2;
    a3 = a3 + b3;
    a4 = a4 + b4;
    
    return (a0, a1, a2, a3, a4);
  }
}.

masking sni 4 M.refresh Byte.ComRing.(+).
