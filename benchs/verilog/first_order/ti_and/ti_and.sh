read_verilog ti_and.v;
hierarchy -check -top mul;
proc;
flatten;
opt;
memory;
opt;
techmap;
opt;
write_ilang ti_and.ilang
