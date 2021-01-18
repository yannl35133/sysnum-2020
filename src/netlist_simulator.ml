open Netlist_ast

exception Simulation_error of string

let debug = ref false
let init_value = ref Off

let print_variable x value =
  Format.printf "Output value for %s : %a\n" x Netlist_printer.print_value value

let print_output env x =
  print_variable x (Env.find x env)

let print_memories x a =
  if Array.length a > 0 then
    Format.printf "Memory for variable %s : %a\n" x (Netlist_printer.print_list Netlist_printer.print_value "[| ""; "" |]") (Array.to_list a)

let init_wires n = VBitArray (Array.make n !init_value)
let unstable_wires n = VBitArray (Array.make n Unstabilized)

let init_mem p =
  let r = Env.empty in
  let add_init r (x, exp) =
    let mem = match exp with
    | Ereg a -> Array.make 1 (
          match Env.find a p.p_vars with
          | TBitArray n -> init_wires n
          )
    | Erom (m, n, _) ->           Array.make (1 lsl m) (init_wires n)
    | Eram (m, n, _, _, _, _) ->  Array.make (1 lsl m) (init_wires n)
    | Earg _|Enot _|Ebinop (_, _, _)|Emux (_, _, _)|Econcat (_, _)|Eslice (_, _, _)|Eselect _ ->
                                  Array.make 0 (init_wires 0)
    in
    Env.add x mem r
  in
  List.fold_left add_init r p.p_eqs

let extract_bit_array = function
  | VBitArray a -> a

let (!!) = extract_bit_array

let extract_bit a =
  if Array.length !!a <> 1 then
    failwith "Wrong arity"
  else
    !!a.(0)

let to_int a =
  Array.fold_right
    (fun b acc -> match b, acc with
      | On, Some acc  -> Some (2 * acc + 1)
      | Off, Some acc -> Some (2 * acc)
      | _ -> None)
    !!a (Some 0)           (* Little-endian *)

let concat a b =
  VBitArray (Array.append !!a !!b)

let compute_arg env = function
  | Aconst c -> c
  | Avar x -> Env.find x env

let compute_binop op a b =
  let f = match op with
    | And ->  (fun a b -> match (a, b) with Off, _ | _, Off -> Off | Unstabilized, _ | _, Unstabilized -> Unstabilized | _ -> On)
    | Or ->   (fun a b -> match (a, b) with On, _  | _, On ->  On  | Unstabilized, _ | _, Unstabilized -> Unstabilized | _ -> Off)
    | Xor ->  (fun a b -> match (a, b) with On, Off | Off, On -> On | Unstabilized, _ | _, Unstabilized -> Unstabilized | _ -> Off)
    | Nand -> (fun a b -> match (a, b) with Off, _ | _, Off -> On  | Unstabilized, _ | _, Unstabilized -> Unstabilized | _ -> On)
  in
  if Array.length !!a = Array.length !!b then
    VBitArray (Array.map2 f !!a !!b)
  else failwith "Wrong arity"

let compute_unop a =
  let f = (function
    | On -> Off
    | Unstabilized -> Unstabilized
    | Off -> On)
  in
  VBitArray (Array.map f !!a)


let compute_mux a b c =
  if Array.length !!b <> Array.length !!c then
    raise (Simulation_error (Format.sprintf "MUX outputs do not have the same size, %d <> %d" (Array.length !!b) (Array.length !!c)));
  match extract_bit a with
  | Off ->          b
  | On ->           c
  | Unstabilized -> unstable_wires (Array.length !!b)


let compute_exp env memory = function
  | Earg a ->
      compute_arg env a
  | Enot a ->
      compute_unop (compute_arg env a)
  | Ebinop (binop, a, b) ->
      compute_binop binop (compute_arg env a) (compute_arg env b)
  | Emux (a, b, c) ->
      compute_mux (compute_arg env a) (compute_arg env b) (compute_arg env c)
  | Econcat (a, b) ->
      concat (compute_arg env a) (compute_arg env b)
  | Eslice (i, j, a) ->
      VBitArray (Array.sub !!(compute_arg env a) i (j - i + 1))
  | Eselect (i, a) ->
      VBitArray (Array.make 1 !!(compute_arg env a).(i))
  | Ereg _ ->
      memory.(0)
  | Erom (_, ws, ra) -> begin
      match to_int @@ compute_arg env ra with
      | Some n -> memory.(n)
      | None   -> unstable_wires ws  (* Corrupted input *)
      end
  | Eram (_, ws, ra, _, _, _) -> begin
      match to_int @@ compute_arg env ra with
      | Some n -> memory.(n)
      | None   -> unstable_wires ws  (* Corrupted input *)
      end

let compute_writes env memory = function
  | Earg _ | Enot _ | Ebinop _ | Emux _ | Econcat _ | Eslice _ | Eselect _ | Erom _ -> ()
  | Ereg x -> memory.(0) <- compute_arg env (Avar x)
  | Eram (_, ws, _, w_flag, wa, data) ->
      match extract_bit @@ compute_arg env w_flag, to_int @@ compute_arg env wa with
      | On, Some n ->           memory.(n) <- compute_arg env data
      | Off, _ ->               ()
      | Unstabilized, Some n -> memory.(n) <- unstable_wires ws      (* Corrupt *)
      | On, None
      | Unstabilized, None ->   Array.iteri (fun i _ -> memory.(i) <- unstable_wires ws) memory (* Corrupt everything ! *)


let linear_update memories env p =
  let update_env env (x, exp) =
    let v = compute_exp env (Env.find x memories) exp in
    if !debug then
      Format.printf "%s = %a\n" x Netlist_printer.print_value v;
    Env.add x v env
  in
  List.fold_left update_env env p.p_eqs

let memory_writes memories env p =
  let update_mem (x, exp) =
    let mem = Env.find x memories in
    compute_writes env mem exp;
  in
  List.iter update_mem p.p_eqs

let get_input env n x =
  let size_prompt = if n <> 1 then
    Format.sprintf "(size %d) " n
    else ""
  in
  Format.printf "Input value for %s %s: " x size_prompt;
  Format.print_flush ();
  let s = Scanf.scanf "%s\n" (fun x -> x) in
  if String.length s <> n then failwith "Wrong length";
  String.iter (fun v -> if v <> '0' && v <> '1' then failwith "Wrong character") s;

  Env.add x (VBitArray (Array.init n (fun i -> if s.[i] = '1' then On else Off ))) env


let simulator ~debug:debug0 ~init program number_steps =
  init_value := init;
  debug := debug0;
  let input env x =
    match Env.find x program.p_vars with
    | TBitArray n -> get_input env n x
  in

  (* Initialize memory cells *)
  let memories = init_mem program in

  (* RAM / ROM *)
  for i = 1 to number_steps do
    Format.printf "## Step number %d@." i;
    let env = List.fold_left input Env.empty program.p_inputs in
    if List.length program.p_inputs > 0 then
      Format.printf "\n";
    let env' = linear_update memories env program in
    memory_writes memories env' program;
    if !debug then
      Env.iter print_memories memories;
    List.iter (print_output env') program.p_outputs;
    Format.printf "\n";
  done

