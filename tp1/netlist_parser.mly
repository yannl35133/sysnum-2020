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
%token AND MUX NAND OR RAM ROM XOR REG NOT
%token CONCAT SELECT SLICE
%token COLON EQUAL COMMA VAR IN INPUT OUTPUT
%token EOF

%start program             /* the entry point */
%type <Netlist_ast.program> program

%%
program:
  INPUT inp=separated_list(COMMA, NAME)
    OUTPUT out=separated_list(COMMA, NAME)
    VAR vars=separated_list(COMMA, var) IN eqs=list(equ) EOF
    { { p_eqs = eqs; p_vars = Env.of_list vars; p_inputs = inp; p_outputs = out; } }

equ:
  x=NAME EQUAL e=exp { (x, e) }

exp:
  | a=arg { Earg a }
  | NOT x=arg { Enot x }
  | REG x=NAME { Ereg x }
  | AND x=arg y=arg { Ebinop(And, x, y) }
  | OR x=arg y=arg { Ebinop(Or, x, y) }
  | NAND x=arg y=arg { Ebinop(Nand, x, y) }
  | XOR x=arg y=arg { Ebinop(Xor, x, y) }
  | MUX x=arg y=arg z=arg { Emux(x, y, z) }
  | ROM addr=int word=int ra=arg
    { Erom(addr, word, ra) }
  | RAM addr=int word=int ra=arg we=arg wa=arg data=arg
    { Eram(addr, word, ra, we, wa, data) }
  | CONCAT x=arg y=arg
     { Econcat(x, y) }
  | SELECT idx=int x=arg
     { Eselect (idx, x) }
  | SLICE min=int max=int x=arg
     { Eslice (min, max, x) }

arg:
  | n=CONST { Aconst (value_of_const n) }
  | id=NAME { Avar id }

var: x=NAME ty=ty_exp { (x, ty) }
ty_exp:
  | /*empty*/   { TBitArray 1 }
  | COLON n=int { TBitArray n }

int:
  | c=CONST { int_of_string c }
