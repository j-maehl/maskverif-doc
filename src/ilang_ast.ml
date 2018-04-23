open Util

type ident = string located

type attribute_arg = 
  | AA_int of int 
  | AA_string of string 

type attribute = ident * attribute_arg 

type 'a with_attribute = {
    attribute : attribute list;
    wa_data : 'a 
  }

type wire_opt = 
  | WO_input  of int
  | WO_output of int 
  | WO_width  of int
  | WO_upto 

type id_index = ident * int option

type expr = 
  | Eid of id_index 
  | Econst of string located

type c_connect = {
    c_connect_lhs : ident;
    c_connect_rhs : expr;
  }
               
type cell = {
    cell_name1: ident;
    cell_name2: ident;
    cell_connect: c_connect list
  }

type wire = wire_opt list * ident 

type rhs = 
  | Rexpr of expr
  | Rvect of expr list

type connect = {
    connect_lhs : rhs; (* ident * (int * int) option; *)
    connect_rhs : rhs 
  }

type shares = 
  | Sident of ident 
  | Svindex of ident * int list 
  | Sindex of ident * int * int 
  | Svect  of id_index list

type decl = 
  | Input  of ident option * shares
  | Output of shares 
  | Random of id_index list
  | Public of id_index list

type mod_decl = 
  | Parameter of ident 
  | Wire of wire
  | Cell of cell
  | Connect of connect located
  | Decl of decl

type module1 = {
    mod_name : ident;
    mod_decl : mod_decl located with_attribute list
  }

type prog = { 
    pautoidx : int;
    module_decl : module1 with_attribute 
  }

