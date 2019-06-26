read_verilog aes.v;
hierarchy -check -top aes_sbox;
proc;
flatten;
opt;
memory;
opt;
techmap;
opt;
write_ilang aes.ilang
