//  -----------------------------------------------------------------------------
//                     Design Information
//  -----------------------------------------------------------------------------
//
//             Author: Begul Bilgin
//
//        Description: Lightweight Authenticated Encryption.
//                     160-bit state; Parallel Threshold Implementation
//
//  -----------------------------------------------------------------------------
module  lae  ( start , inp1, inp2, inp3, inp4, key1, key2, key3, key4, nonce, Ain, Min, last, ck , cout1, cout2, cout3, cout4, final_o, Tout1, Tout2, Tout3, Tout4, getdata) ;


input   start ;
input    [11:0]  inp1;
input    [11:0]  inp2;
input    [11:0]  inp3;
input    [11:0]  inp4;
input    [95:0]  key1;
input    [95:0]  key2;
input    [95:0]  key3;
input    [95:0]  key4;
input    [47:0]  nonce;
input    ck ; // Rising edge clock
input    Ain ;
input    Min ;
input    last ;

output    final_o;
output    [11:0] cout1 ;
output    [11:0] cout2 ;
output    [11:0] cout3 ;
output    [11:0] cout4 ;
output    [95:0] Tout1 ;  // Data output
output    [95:0] Tout2 ;
output    [95:0] Tout3 ;
output    [95:0] Tout4 ;
output    getdata;

wire    init ;
wire    getdata ;
wire    outc ;
wire    final_o ;
wire    [5:0]   rcon;

lae_control  u_control  (
  .start   ( start   ) ,
  .ck      ( ck      ) ,
  .Ain     ( Ain     ) ,
  .Min     ( Min     ) ,
  .last    ( last    ) ,
  .init    ( init    ) ,
  .getdata ( getdata ) ,
  .outc    ( outc    ) ,
  .final_o   ( final_o   ) ,
  .rcon    (rcon     )
) ;

lae_comb  u_comb  (
  .ck      ( ck       ), 
  .inp1    ( inp1     ), 
  .inp2    ( inp2     ), 
  .inp3    ( inp3     ), 
  .inp4    ( inp4     ), 
  .key1    ( key1     ),
  .key2    ( key2     ),
  .key3    ( key3     ),
  .key4    ( key4     ),
  .nonce   ( nonce    ), 
  .init    ( init     ) ,
  .getdata ( getdata  ) ,
  .outc    ( outc     ) ,
  .final_o   ( final_o    ) ,
  .rcon    (rcon      ),
  .c1      (cout1     ),
  .c2      (cout2     ),
  .c3      (cout3     ),
  .c4      (cout4     ),
  .T1      (Tout1     ),
  .T2      (Tout2     ),
  .T3      (Tout3     ),
  .T4      (Tout4     )
) ;

endmodule
