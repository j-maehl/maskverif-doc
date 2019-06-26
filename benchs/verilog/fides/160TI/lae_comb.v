//  -----------------------------------------------------------------------------
//                     Design Information
//  -----------------------------------------------------------------------------
//
//             Author: Begul Bilgin
//
//        Description: 4x8x5-bit Combinational Logic
//
//  -----------------------------------------------------------------------------
module  lae_comb  (ck, inp1, inp2, inp3, inp4, key1, key2, key3, key4, nonce, init, getdata, getdata0, outc, final_o, rcon, c1, c2, c3, c4, T1, T2, T3, T4 ) ;

input ck;
input    [9:0]  inp1;
input    [9:0]  inp2;
input    [9:0]  inp3;
input    [9:0]  inp4;
input    [79:0]  key1;
input    [79:0]  key2;
input    [79:0]  key3;
input    [79:0]  key4;
input    [39:0]  nonce;
input    init;
input    getdata;
input    getdata0;
input    outc;
input    final_o;
input    [4:0] rcon;

output  [9:0]   c1;
output  [9:0]   c2;
output  [9:0]   c3;
output  [9:0]   c4;
output  [79:0]  T1;
output  [79:0]  T2;
output  [79:0]  T3;
output  [79:0]  T4;

reg   [159:0] state1;
reg   [159:0] state2;
reg   [159:0] state3;
reg   [159:0] state4;
wire  [159:0] sbin1;
wire  [159:0] sbin2;
wire  [159:0] sbin3;
wire  [159:0] sbin4;
wire  [159:0] sbout1;
wire  [159:0] sbout2;
wire  [159:0] sbout3;
wire  [159:0] sbout4;
wire  [159:0] shrow1;
wire  [159:0] shrow2;
wire  [159:0] shrow3;
wire  [159:0] shrow4;
wire  [159:0] mcout1;
wire  [159:0] mcout2;
wire  [159:0] mcout3;
wire  [159:0] mcout4;

//State
assign sbin1 = init ? {key1, nonce, 40'd0} : getdata0 ? state1^{inp1[9:5], key1[79:65], inp1[4:0], key1[64:0], 30'd0} : getdata ? state1^{inp1[9:5], 15'd0, inp1[4:0], 134'd0} : state1;
assign sbin2 = init ? {key2, 40'd0, 40'd0} : getdata0 ? state2^{inp2[9:5], key2[79:65], inp2[4:0], key2[64:0], 30'd0} : getdata ? state2^{inp2[9:5], 15'd0, inp2[4:0], 134'd0} : state2;
assign sbin3 = init ? {key3, 40'd0, 40'd0} : getdata0 ? state3^{inp3[9:5], key3[79:65], inp3[4:0], key3[64:0], 30'd0} : getdata ? state3^{inp3[9:5], 15'd0, inp3[4:0], 134'd0} : state3;
assign sbin4 = init ? {key4, 40'd0, 40'd0} : getdata0 ? state4^{inp4[9:5], key4[79:65], inp4[4:0], key4[64:0], 30'd0} : getdata ? state4^{inp4[9:5], 15'd0, inp4[4:0], 134'd0} : state4;

//SubBytes
sbox_state sb(.a1(sbin1), .a2(sbin2), .a3(sbin3), .a4(sbin4), .y1(sbout1), .y2(sbout2), .y3(sbout3), .y4(sbout4));
//ShiftRows
assign shrow1={sbout1[159:155], sbout1[134:130], sbout1[109:105], sbout1[4:0], sbout1[139:135], sbout1[114:110], sbout1[89:85], sbout1[144:140], sbout1[119:115], sbout1[94:90], sbout1[69:65], sbout1[124:120], sbout1[99:95], sbout1[74:70], sbout1[49:45], sbout1[104:100], sbout1[79:75], sbout1[54:50], sbout1[29:25], sbout1[84:80], sbout1[59:55], sbout1[34:30], sbout1[9:5], sbout1[44:40], sbout1[39:35], sbout1[14:10], sbout1[149:145], sbout1[44:40], sbout1[19:15], sbout1[154:150], sbout1[129:125], sbout1[24:20]};
assign shrow2={sbout2[159:155], sbout2[134:130], sbout2[109:105], sbout2[4:0], sbout2[139:135], sbout2[114:110], sbout2[89:85], sbout2[144:140], sbout2[119:115], sbout2[94:90], sbout2[69:65], sbout2[124:120], sbout2[99:95], sbout2[74:70], sbout2[49:45], sbout2[104:100], sbout2[79:75], sbout2[54:50], sbout2[29:25], sbout2[84:80], sbout2[59:55], sbout2[34:30], sbout2[9:5], sbout2[44:40], sbout2[39:35], sbout2[14:10], sbout2[149:145], sbout2[44:40], sbout2[19:15], sbout2[154:150], sbout2[129:125], sbout2[24:20]};
assign shrow3={sbout3[159:155], sbout3[134:130], sbout3[109:105], sbout3[4:0], sbout3[139:135], sbout3[114:110], sbout3[89:85], sbout3[144:140], sbout3[119:115], sbout3[94:90], sbout3[69:65], sbout3[124:120], sbout3[99:95], sbout3[74:70], sbout3[49:45], sbout3[104:100], sbout3[79:75], sbout3[54:50], sbout3[29:25], sbout3[84:80], sbout3[59:55], sbout3[34:30], sbout3[9:5], sbout3[44:40], sbout3[39:35], sbout3[14:10], sbout3[149:145], sbout3[44:40], sbout3[19:15], sbout3[154:150], sbout3[129:125], sbout3[24:20]};
assign shrow4={sbout4[159:155], sbout4[134:130], sbout4[109:105], sbout4[4:0], sbout4[139:135], sbout4[114:110], sbout4[89:85], sbout4[144:140], sbout4[119:115], sbout4[94:90], sbout4[69:65], sbout4[124:120], sbout4[99:95], sbout4[74:70], sbout4[49:45], sbout4[104:100], sbout4[79:75], sbout4[54:50], sbout4[29:25], sbout4[84:80], sbout4[59:55], sbout4[34:30], sbout4[9:5], sbout4[44:40], sbout4[39:35], sbout4[14:10], sbout4[149:145], sbout4[44:40], sbout4[19:15], sbout4[154:150], sbout4[129:125], sbout4[24:20]};
//MixColumns
mcol_state  mc1(.a(shrow1), .y(mcout1) );
mcol_state  mc2(.a(shrow2), .y(mcout2) );
mcol_state  mc3(.a(shrow3), .y(mcout3) );
mcol_state  mc4(.a(shrow4), .y(mcout4) );

always  @ ( posedge ck ) state1  <=  mcout1^{rcon, 155'd0} ;
always  @ ( posedge ck ) state2  <=  mcout2^{rcon, 155'd0} ;
always  @ ( posedge ck ) state3  <=  mcout3^{rcon, 155'd0} ;
always  @ ( posedge ck ) state4  <=  mcout4^{rcon, 155'd0} ;

assign c1=outc ? state1[159:150]^inp1 : 10'd0;
assign c2=outc ? state2[159:150]^inp2 : 10'd0;
assign c3=outc ? state3[159:150]^inp3 : 10'd0;
assign c4=outc ? state4[159:150]^inp4 : 10'd0;

assign T1=final_o ? state1[159:80] : 80'd0;
assign T2=final_o ? state2[159:80] : 80'd0;
assign T3=final_o ? state3[159:80] : 80'd0;
assign T4=final_o ? state4[159:80] : 80'd0;

endmodule
