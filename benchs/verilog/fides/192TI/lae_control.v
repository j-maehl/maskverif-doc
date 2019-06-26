//  -----------------------------------------------------------------------------
//                     Design Information
//  -----------------------------------------------------------------------------
//
//             Author: Begul Bilgin
//
//        Description: 4x8x5-bit Control File
//
//  -----------------------------------------------------------------------------
module  lae_control  ( start , ck , Ain, Min, last, init, getdata, outc, final_o, rcon ) ;

input   start ;
input   Ain ;
input   Min ;
input   last ;
input   ck ; // Rising edge clock

output    init;
output    getdata;
output    outc;
output    final_o;
output    [5:0] rcon;

reg     [3:0]  round_ps;
reg     first;
// Define counter
//
always  @ ( posedge ck  )  round_ps  <= start ? 0 : (first || last) ? round_ps+1 : round_ps;
always  @ ( posedge ck  )  first <= start ? 1 : (round_ps==15) ? 0 : first;
assign  init   =  (first&&round_ps==0) ;
assign getdata = (~first&& round_ps==0) ? 1 : 0;
assign rcon={2'b0, round_ps};
assign outc=Min;
assign final_o= (last&&round_ps==15);

endmodule
