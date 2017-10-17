type gf.

op of_int : int -> gf.
op [-] : gf -> gf.
op inv : gf -> gf.

op ( + ) : gf -> gf -> gf.
op ( - ) : gf -> gf -> gf. 
op ( * ) : gf -> gf -> gf. 
op ( / ) : gf -> gf -> gf.

op sample : gf distr.

op L2 = of_int 3.

op divL2 x = x / L2.
op mulL2 x = x * L2.

module M2 = {
  proc mul (a1,a2, b1,b2:gf) = {
    var c1, c2, r, aux1, aux2: gf;

    c1 <- a1 * b1; (* c1 <- c1 * L1; *)
    c2 <- a2 * b2; c2 <- c2 * L2;

    r <$ sample;  
    aux1 <- r (* / L1 *);
    aux2 <- a1 * b2;
    aux2 <- aux2 (* * L1 *);
    aux2 <- aux2 + aux1;
    c1   <- c1 + aux2;
    aux1 <- divL2 r ;
    aux2 <- a2 * b1;
    aux2 <- aux2 * L2;
    aux2 <- aux2 - aux1;
    c2   <- c2 + aux2;

    return (c1,c2);
  }

  proc main (a, b:gf) = {
    var a1,a2,b1,b2,c1,c2:gf;
    a2 <$ sample;    
    a1 <- a - mulL2 a2;

    b2 <$ sample;
    b1 <- b - mulL2 b2;


    (c1,c2) = mul(a1,a2, b1,b2);  
    return (c1,c2);
  }  
}.

masking bmoment 2 2 M2.main ( + ) (-) divL2 mulL2. 

rnd_indep fail on (a22 * b22, a - mulL2 a22)
found expectation 14280 for (a,0) 
and 14400 for (a,1) 


module M3 = {
  proc mul (a0,a1,a2, b0,b1,b2:gf) = {
    var c0, c1, c2, r, aux1, aux2: gf;

    (*
      <L,A> = a  = L0 a0 + L1 a1 + L2 a2
      <L,B> = b  = L0 b0 + L1 b1 + L2 b2
      <L,C> = c  = a * b = 
        (L0 a0 + L1 a1 + L2 a2) * (L0 b0 + L1 b1 + L2 b2)
        L0 * a0 (L0 b0 + L1 b1 + L2 b2) + 
        L1 * a1 (L0 b0 + L1 b1 + L2 b2) + 
        L2 * a2 (L0 b0 + L1 b1 + L2 b2)
      
        
      for i = 0 to n do
         
    *)

    c0 <- a0 * b0; c0 <- c0 * L0;
    c1 <- a1 * b1; c1 <- c1 * L1;
    c2 <- a2 * b2; c2 <- c2 * L2;

    r <$ sample;  
    aux1 <- divL0 r ;
    aux2 <- a0 * b1;
    aux2 <- aux2 * L1;
    aux2 <- aux2 + aux1;
    c0   <- c0 + aux2;
    aux1 <- divL1 r ;
    aux2 <- a1 * b0;
    aux2 <- aux2 * L1;
    aux2 <- aux2 - aux1;
    c1   <- c1 + aux2;

(*    r <$ sample;  
    aux1 <- divL0 r; 
    aux2 <- a0 * b2;
    aux2 <- aux2 * L2;
    aux2 <- aux2 + aux1;
    c0   <- c0 + aux2;
    aux1 <- divL2 r ;
    aux2 <- a2 * b0;
    aux2 <- aux2 * L2;
    aux2 <- aux2 - aux1;
    c2   <- c2 + aux2;

    r <$ sample;  
    aux1 <- divL1 r;
    aux2 <- a1 * b2;
    aux2 <- aux2 * L2;
    aux2 <- aux2 + aux1;
    c1   <- c1 + aux2;
    aux1 <- divL2 r;
    aux2 <- a2 * b1;
    aux2 <- aux2 * L2;
    aux2 <- aux2 - aux1;
    c2   <- c2 + aux2; *)

    return (c0,c0,c0);
  }

  proc main (a, b:gf) = {
    var a0,a1,a2,b0,b1,b2,c0,c1,c2:gf;
    a0 <$ sample;
    a1 <$ sample;
    a2 <- divL2 (a - divL0 a0 - divL1 a1);

    b0 <$ sample;
    b1 <$ sample;
    b2 <- divL2 (b - divL0 b0 - divL1 b1);

    (c0,c1,c2) = mul(a0,a1,a2, b0,b1,b2);
    return (c0,c1,c2);
  } 
}.




    

    
    

    
 
