type ident = string

(* Environment using ident as key *)
module Env = struct
  include Map.Make(struct
    type t = ident
    let compare = compare
  end)

  let of_list l =
    List.fold_left (fun env (x, ty) -> add x ty env) empty l
end

type ty = TBitArray of int

type wire_value =
  | On
  | Off
  | Unstabilized

type value =
  | VBitArray of wire_value array

type binop = Or | Xor | And | Nand

(* argument of operators (variable or constant) *)
type arg =
  | Avar of ident                         (* x *)
  | Aconst of value                       (* constant *)

(* Expressions (see MiniJazz documentation for more info on the operators) *)
type exp =
  | Earg of arg                           (* a: argument *)
  | Ereg of ident                         (* REG x : register *)
  | Enot of arg                           (* NOT a (possibly n-ary) *)
  | Ebinop of binop * arg * arg           (* OP a1 a2 : boolean operation (possibly n-ary) *)
  | Emux of arg * arg * arg               (* MUX c a1 a2 : multiplexer c a1 + ~c a2; if c then a1 else a2. a1 and a2 may have any size *)
  | Erom of int (*addr size*) * int (*word size*) * arg (*read_addr*)
                                          (* ROM addr_size word_size read_addr *)
  | Eram of int (*addr size*) * int (*word size*)
      * arg (*read_addr*) * arg (*write_enable*)
      * arg (*write_addr*) * arg (*data*)
                                          (* RAM addr_size word_size read_addr write_enable write_addr data *)
  | Econcat of arg * arg                  (* CONCAT a1 a2 : concatenation of arrays *)
  | Eslice of int * int * arg             (* SLICE i1 i2 a : extract the slice of a between indices i1 and i2 (inclusive) *)
  | Eselect of int * arg                  (* SELECT i a : ith element of a (0-indexed) *)

(* equations: x = exp *)
type equation = ident * exp

type program =
  { p_eqs : equation list;                (* equations *)
    p_inputs : ident list;                (* inputs *)
    p_outputs : ident list;               (* outputs *)
    p_vars : ty Env.t; }                  (* maps variables to their types*)
