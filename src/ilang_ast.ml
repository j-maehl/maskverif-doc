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

type shares_by = (int * int) option

type wire_opt = 
  | WO_public of int 
  | WO_input  of (int * shares_by)
  | WO_output of (int * shares_by)
  | WO_random of int 
  | WO_width  of int

type index = int * int

type id_index = ident * index option

type expr = 
  | Eid of (ident * int option)
  | Econst of string

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
    connect_lhs : ident * (int * int) option;
    connect_rhs : rhs 
  }

type mod_decl = 
  | Wire of wire
  | Cell of cell
  | Connect of connect

type module1 = {
    mod_name : ident;
    mod_decl : mod_decl with_attribute list
  }
type prog = { 
    pautoidx : int;
    module_decl : module1 with_attribute 
  }
