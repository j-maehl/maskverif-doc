read_verilog Sbox_1.v;
read_verilog Sbox_2.v;
read_verilog Sbox_3.v;
read_verilog Sbox_4.v;
read_verilog Sbox_state.v;
hierarchy -check -top sbox_state;
proc;
flatten;
opt;
memory;
opt;
techmap;
opt;
write_ilang Sbox_state.ilang
