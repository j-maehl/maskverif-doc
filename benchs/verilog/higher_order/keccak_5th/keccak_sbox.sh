read_verilog keccak_sbox_wo.v;
hierarchy -check -top keccak_sbox;
proc;
flatten;
opt;
memory;
opt;
techmap;
opt;
write_ilang keccak_sbox.ilang
