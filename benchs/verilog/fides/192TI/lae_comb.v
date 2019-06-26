//  -----------------------------------------------------------------------------
//                     Design Information
//  -----------------------------------------------------------------------------
//
//             Author: Begul Bilgin
//
//        Description: 4x8x5-bit Combinational Logic
//
//  -----------------------------------------------------------------------------
module  lae_comb  (ck, inp1, inp2, inp3, inp4, key1, key2, key3, key4, nonce, init, getdata,outc, final_o, rcon, c1, c2, c3, c4, T1, T2, T3, T4 ) ;

input ck;
input    [11:0]  inp1;
input    [11:0]  inp2;
input    [11:0]  inp3;
input    [11:0]  inp4;
input    [95:0]  key1;
input    [95:0]  key2;
input    [95:0]  key3;
input    [95:0]  key4;
input    [47:0]  nonce;
input    init;
input    getdata;
input    outc;
input    final_o;
input    [5:0] rcon;

output  [11:0]   c1;
output  [11:0]   c2;
output  [11:0]   c3;
output  [11:0]   c4;
output  [95:0]  T1;
output  [95:0]  T2;
output  [95:0]  T3;
output  [95:0]  T4;

reg   [191:0] state1;
reg   [191:0] state2;
reg   [191:0] state3;
reg   [191:0] state4;
wire  [191:0] sbin1;
wire  [191:0] sbin2;
wire  [191:0] sbin3;
wire  [191:0] sbin4;
wire  [191:0] sbout1;
wire  [191:0] sbout2;
wire  [191:0] sbout3;
wire  [191:0] sbout4;
wire  [191:0] shrow1;
wire  [191:0] shrow2;
wire  [191:0] shrow3;
wire  [191:0] shrow4;
wire  [191:0] mcout1;
wire  [191:0] mcout2;
wire  [191:0] mcout3;
wire  [191:0] mcout4;

//State
assign sbin1 = init ? {key1, nonce, 40'd0} : getdata ? state1^{inp1[11:6], key1[95:78], inp1[5:0], key1[77:0], 30'd0} : state1;
assign sbin2 = init ? {key2, 40'd0, 40'd0} : getdata ? state2^{inp2[11:6], key2[95:78], inp2[5:0], key2[77:0], 30'd0} : state2;
assign sbin3 = init ? {key3, 40'd0, 40'd0} : getdata ? state3^{inp3[11:6], key3[95:78], inp3[5:0], key3[77:0], 30'd0} : state3;
assign sbin4 = init ? {key4, 40'd0, 40'd0} : getdata ? state4^{inp4[11:6], key4[95:78], inp4[5:0], key4[77:0], 30'd0} : state4;

//SubBytes
sbox_state sb(.a1(sbin1), .a2(sbin2), .a3(sbin3), .a4(sbin4), .y1(sbout1), .y2(sbout2), .y3(sbout3), .y4(sbout4));
//ShiftRows
assign shrow1={sbout1[191:186], sbout1[161:156], sbout1[131:126], sbout1[5:0], sbout1[167:162], sbout1[137:132], sbout1[107:102], sbout1[173:168], sbout1[143:138], sbout1[113:108], sbout1[83:78], sbout1[149:144], sbout1[119:114], sbout1[89:84], sbout1[59:54], sbout1[125:120], sbout1[95:90], sbout1[65:60], sbout1[35:30], sbout1[101:96], sbout1[71:66], sbout1[41:36], sbout1[11:6], sbout1[77:72], sbout1[47:42], sbout1[17:12], sbout1[179:174], sbout1[53:48], sbout1[23:18], sbout1[185:180], sbout1[155:150], sbout1[29:24]};
assign shrow2={sbout2[191:186], sbout2[161:156], sbout2[131:126], sbout2[5:0], sbout2[167:162], sbout2[137:132], sbout2[107:102], sbout2[173:168], sbout2[143:138], sbout2[113:108], sbout2[83:78], sbout2[149:144], sbout2[119:114], sbout2[89:84], sbout2[59:54], sbout2[125:120], sbout2[95:90], sbout2[65:60], sbout2[35:30], sbout2[101:96], sbout2[71:66], sbout2[41:36], sbout2[11:6], sbout2[77:72], sbout2[47:42], sbout2[17:12], sbout2[179:174], sbout2[53:48], sbout2[23:18], sbout2[185:180], sbout2[155:150], sbout2[29:24]};
assign shrow3={sbout3[191:186], sbout3[161:156], sbout3[131:126], sbout3[5:0], sbout3[167:162], sbout3[137:132], sbout3[107:102], sbout3[173:168], sbout3[143:138], sbout3[113:108], sbout3[83:78], sbout3[149:144], sbout3[119:114], sbout3[89:84], sbout3[59:54], sbout3[125:120], sbout3[95:90], sbout3[65:60], sbout3[35:30], sbout3[101:96], sbout3[71:66], sbout3[41:36], sbout3[11:6], sbout3[77:72], sbout3[47:42], sbout3[17:12], sbout3[179:174], sbout3[53:48], sbout3[23:18], sbout3[185:180], sbout3[155:150], sbout3[29:24]};
assign shrow4={sbout4[191:186], sbout4[161:156], sbout4[131:126], sbout4[5:0], sbout4[167:162], sbout4[137:132], sbout4[107:102], sbout4[173:168], sbout4[143:138], sbout4[113:108], sbout4[83:78], sbout4[149:144], sbout4[119:114], sbout4[89:84], sbout4[59:54], sbout4[125:120], sbout4[95:90], sbout4[65:60], sbout4[35:30], sbout4[101:96], sbout4[71:66], sbout4[41:36], sbout4[11:6], sbout4[77:72], sbout4[47:42], sbout4[17:12], sbout4[179:174], sbout4[53:48], sbout4[23:18], sbout4[185:180], sbout4[155:150], sbout4[29:24]};
//MixColumns
mcol_state  mc1(.a(shrow1), .y(mcout1) );
mcol_state  mc2(.a(shrow2), .y(mcout2) );
mcol_state  mc3(.a(shrow3), .y(mcout3) );
mcol_state  mc4(.a(shrow4), .y(mcout4) );

always  @ ( posedge ck ) state1  <=  mcout1^{rcon, 186'd0} ;
always  @ ( posedge ck ) state2  <=  mcout2;
always  @ ( posedge ck ) state3  <=  mcout3;
always  @ ( posedge ck ) state4  <=  mcout4;

assign c1=outc ? state1[159:150]^inp1 : 12'd0;
assign c2=outc ? state2[159:150]^inp2 : 12'd0;
assign c3=outc ? state3[159:150]^inp3 : 12'd0;
assign c4=outc ? state4[159:150]^inp4 : 12'd0;

assign T1=final_o ? state1[191:95] : 96'd0;
assign T2=final_o ? state2[159:80] : 80'd0;
assign T3=final_o ? state3[159:80] : 80'd0;
assign T4=final_o ? state4[159:80] : 80'd0;

endmodule
