let print_only = ref false
let number_steps = ref (-1)
let filename = ref ""


let compile filename =
  let ic = Netlist.read_file filename in
  let lexbuf = Lexing.from_channel ic in
  let out_file = (Filename.chop_suffix filename ".net") ^ "_scheduled.net" in
  let oc = open_out out_file in
  try
    let program = Netlist_parser.program Netlist_lexer.token lexbuf in
    let scheduled_program = Scheduler.schedule program in

    Netlist.print_program oc scheduled_program;
    close_out oc;
    
    if not !print_only then
      let cmd = Format.sprintf "./prebuilt_netlist_simulator.byte %s %s"
        (
          if !number_steps = -1 then ""
          else Format.sprintf "-n %d" !number_steps
        )
        out_file
      in
      ignore (Unix.system cmd)
  with
    | Netlist_parser.Error ->
        Format.eprintf "@[<v>%a@ Syntax error@]"
        Netlist.localisation Lexing.(lexeme_start_p lexbuf, lexeme_end_p lexbuf);
        close_out oc;
        exit 1

    | Scheduler.Combinational_cycle ->
        Format.eprintf "Error: the netlist has a dependency cycle.";
        close_out oc;
        exit 1

let main () =
  Arg.parse
    [
      "-n", Arg.Set_int number_steps, "Number of steps to simulate";
      "--print-only", Arg.Set print_only, "Print the scheduled netlist and return"
    ]
    (fun s -> filename := s)
    "";
    compile !filename


let () = main ()
