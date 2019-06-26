read_verilog keccak_sbox.v;
hierarchy -check -top keccak_sbox;
proc;
flatten;
opt;
memory;
opt;
techmap;
opt;
write_ilang keccak_sbox.ilang
