//  -----------------------------------------------------------------------------
//                     Design Information
//  -----------------------------------------------------------------------------
//
//             Author: Begul Bilgin
//
//        Description: One Instance of Mix Columns for One Element
//
//  -----------------------------------------------------------------------------
module  mcolrow  ( a1,a2,a3, y ) ; 

input   [5:0]  a1 ;  // 5-bit input
input   [5:0]  a2 ;  // 5-bit input
input   [5:0]  a3 ;  // 5-bit input

output  [5:0]  y ;  // 5-bit output

assign y=a1^a2^a3;

endmodule
