read_verilog sbox_3rd_order.v;
hierarchy -check -top bSbox;
proc;
flatten;
opt;
memory;
opt;
techmap;
opt;
write_ilang aes.ilang
