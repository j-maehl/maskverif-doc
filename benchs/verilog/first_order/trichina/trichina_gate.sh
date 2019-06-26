read_verilog trichina_gate.v;
hierarchy -check -top isw_and;
proc;
flatten;
opt;
memory;
opt;
techmap;
opt;
write_ilang trichina_gate.ilang
