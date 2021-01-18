{
open Lexing
open Netlist_parser

let keyword_table = Hashtbl.create 17
let () = List.iter (fun (k, v) -> Hashtbl.add keyword_table k v)
  [
    "AND",    AND;
    "CONCAT", CONCAT;
    "IN",     IN;
    "INPUT",  INPUT;
    "MUX",    MUX;
    "NAND",   NAND;
    "NOT",    NOT;
    "OR",     OR;
    "OUTPUT", OUTPUT;
    "RAM",    RAM;
    "REG",    REG;
    "ROM",    ROM;
    "SELECT", SELECT;
    "SLICE",  SLICE;
    "VAR",    VAR;
    "XOR",    XOR;
  ]

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = alpha | digit | ['-' ''' '_']
let filename = alphanum | '.'

rule token = parse
  | '\n'                              { new_line lexbuf; token lexbuf }     (* skip blanks *)
  | [' ' '\t']                        { token lexbuf }                      (* skip blanks *)
  | "="                               { EQUAL }
  | ":"                               { COLON }
  | ","                               { COMMA }
  | digit+ as lxm                     { CONST lxm }
  | "\"" (filename+ as file) "\""     { FILE file }
  | ('_' ? alpha alphanum* as id)     { try Hashtbl.find keyword_table id
                                        with Not_found -> NAME id }
  | eof                               { EOF }
