open Lexing
open Netlist_ast

exception Parse_error of string

let print_only = ref false
let number_steps = ref (-1)
let debug = ref false
let init_value = ref Off
let filename = ref ""

(* Localize an error given its beginning and ending positions *)
let localisation fmt (pos1, pos2) =
  let l = pos1.pos_lnum in
  let c1 = pos1.pos_cnum - pos1.pos_bol + 1
  and c2 = pos2.pos_cnum - pos1.pos_bol + 1 in
  Format.fprintf fmt "File \"%s\", line %d, characters %d-%d:" !filename l c1 c2

let read_file ?(raise_error=None) filename =
  try
    open_in filename
  with
    Sys_error _ ->
      match raise_error with
      | Some e -> raise e
      | None ->
          Format.eprintf "Error: file \"%s\" has not been found@." filename;
          exit 1


(** [print_program oc p] prints the program [p] on the output channel [oc].
    For instance, to print on the standard output, use [print_program stdout p].
    To print to a file named 'filename', use the following:
        let out = open_out filename in
        print_program out p
*)
let print_program = Netlist_printer.print_program


let compile filename =
  let ic = read_file filename in
  let lexbuf = from_channel ic in
  try
    let program = Netlist_parser.program Netlist_lexer.token lexbuf in

    Typer.check program;

    let scheduled_program = Scheduler.schedule program in

    if !number_steps < 0 then
      number_steps := max_int;

    if !print_only then
      print_program stdout program
    else
      Netlist_simulator.simulator ~debug:!debug ~init:!init_value scheduled_program !number_steps
  with
    | Netlist_parser.Error ->
        Format.eprintf "@[<v>%a@ Syntax error@]@."
        localisation (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf);
        exit 1

    | Scheduler.Combinational_cycle ->
        Format.eprintf "Error: the netlist has a dependency cycle@.";
        exit 1
    
    | Netlist_simulator.Simulation_error s ->
        Format.eprintf "Simulation error: %s@." s;
        exit 1

let main () =
  Arg.parse
    [
      "-n", Arg.Set_int number_steps, "Number of steps to simulate";
      "-u", Arg.Unit (fun () -> init_value := Unstabilized), "Initialises the wires to an unstable state";
      "-i", Arg.Unit (fun () -> init_value := On), "Initialises the wires to 1";
      "--debug", Arg.Set debug, "Activate debug mode";
      "--print-only", Arg.Set print_only, "Print the netlist and return"
    ]
    (fun s -> filename := s)
    "";
    compile !filename


let () = main ()