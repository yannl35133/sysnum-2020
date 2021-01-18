%{
 open Netlist_ast

 let wire_of_string s = match s with
  | "t" | "1" -> On
  | "f" | "0" -> Off
  | "u"       -> Unstabilized
  | _ -> raise Parsing.Parse_error

 let wire_array_of_string s =
   let a = Array.make (String.length s) Unstabilized in
   for i = 0 to String.length s - 1 do
     a.(i) <- wire_of_string (String.sub s i 1)
   done;
   a

 let value_of_const s =
   let n = String.length s in
   if n = 0 then
     raise Parsing.Parse_error
   else
     VBitArray (wire_array_of_string s)
%}

%token <string> CONST
%token <string> NAME
%token <string> FILE
%token AND MUX NAND OR RAM ROM XOR REG NOT
%token CONCAT SELECT SLICE
%token COLON ":" EQUAL "=" COMMA ","
%token VAR IN INPUT OUTPUT
%token EOF

%start<Netlist_ast.program> program             /* the entry point */

%%
program:
  INPUT   p_inputs = separated_list(",", NAME)
  OUTPUT p_outputs = separated_list(",", NAME)
  VAR         vars = separated_list(",", var)
  IN         p_eqs = list(equ)
  EOF                               { { p_eqs; p_vars = Env.of_list vars; p_inputs; p_outputs } }

equ:
  x=NAME "=" e=exp                  { (x, e) }

exp:
  | a=arg                           { Earg a }
  | NOT x=arg                       { Enot x }
  | op=binop x=arg y=arg            { Ebinop (op, x, y) }
  | MUX x=arg y=arg z=arg           { Emux (x, y, z) }
  | CONCAT x=arg y=arg              { Econcat (x, y) }
  | SELECT idx=int x=arg            { Eselect (idx, x) }
  | SLICE min=int max=int x=arg     { Eslice (min, max, x) }
  | REG x=NAME                      { Ereg x }
  | ROM addr=int word=int file=FILE? ra=arg
                                    { Erom (addr, word, file, ra) }
  | RAM addr=int word=int file=FILE? ra=arg we=arg wa=arg data=arg
                                    { Eram (addr, word, file, ra, we, wa, data) }

%inline binop:
  | AND                             { And }
  | OR                              { Or }
  | NAND                            { Nand }
  | XOR                             { Xor }

arg:
  | n=CONST                         { Aconst (value_of_const n) }
  | id=NAME                         { Avar id }

var: x=NAME ty=ty_exp               { (x, ty) }
ty_exp:
  | /*empty*/                       { TBitArray 1 }
  | ":" n=int                       { TBitArray n }

int:
  | c=CONST                         { int_of_string c }
