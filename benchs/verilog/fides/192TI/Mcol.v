//  -----------------------------------------------------------------------------
//                     Design Information
//  -----------------------------------------------------------------------------
//
//             Author: Begul Bilgin
//
//        Description: One Instance of Mix Columns
//
//  -----------------------------------------------------------------------------
module  mcol  ( a1, a2, a3, a4, y1, y2, y3, y4 ) ;


input   [5:0]  a1 ;  // 5-bit input
input   [5:0]  a2 ;  // 5-bit input
input   [5:0]  a3 ;  // 5-bit input
input   [5:0]  a4 ;  // 5-bit input

output  [5:0]  y1 ;  // 5-bit yput
output  [5:0]  y2 ;  // 5-bit yput
output  [5:0]  y3 ;  // 5-bit yput
output  [5:0]  y4 ;  // 5-bit yput

mcolrow m1(.a1(a2), .a2(a3), .a3(a4), .y(y1));
mcolrow m2(.a1(a1), .a2(a3), .a3(a4), .y(y2));
mcolrow m3(.a1(a1), .a2(a2), .a3(a4), .y(y3));
mcolrow m4(.a1(a1), .a2(a2), .a3(a3), .y(y4));

endmodule
