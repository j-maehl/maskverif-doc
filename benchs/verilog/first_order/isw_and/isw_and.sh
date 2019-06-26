read_verilog isw_and.v;
hierarchy -check -top mul_isw;
proc;
flatten;
opt;
memory;
opt;
techmap;
opt;
write_ilang isw_and.ilang
