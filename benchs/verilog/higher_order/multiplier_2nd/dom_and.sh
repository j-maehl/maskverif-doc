read_verilog dom_and_2nd_order.v;
hierarchy -check -top dom_and;
proc;
flatten;
opt;
memory;
opt;
techmap;
opt;
write_ilang dom_and.ilang
