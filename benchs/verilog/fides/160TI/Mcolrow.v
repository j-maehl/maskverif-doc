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

input   [4:0]  a1 ;  // 5-bit input
input   [4:0]  a2 ;  // 5-bit input
input   [4:0]  a3 ;  // 5-bit input

output  [4:0]  y ;  // 5-bit output

assign y=a1^a2^a3;

endmodule
