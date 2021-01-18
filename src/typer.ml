open Netlist_ast

let check program =

  let print_var fmt = function
    | Avar id -> Format.fprintf fmt "%s" id
    | Aconst a -> Format.fprintf fmt "%a" Netlist_printer.print_value a
  in

  let get_size = function
    | Avar id -> let TBitArray n = Env.find id program.p_vars in n
    | Aconst VBitArray a -> Array.length a
  in
  Option.fold ~none:()
    ~some:(fun name -> Format.eprintf "Input variable \"%s\" has not been found among declared variables@." name; exit 1)
    (List.find_opt (fun e -> not @@ Env.mem e program.p_vars) program.p_inputs);

  Option.fold ~none:()
    ~some:(fun name -> Format.eprintf "Output variable \"%s\" has not been found among declared variables@." name; exit 1)
    (List.find_opt (fun e -> not @@ Env.mem e program.p_vars) program.p_outputs);

  Option.fold ~none:()
    ~some:(fun (name, _) -> Format.eprintf "Equation left-hand side \"%s\" has not been found among declared variables@." name; exit 1)
    (List.find_opt (fun (e, _) -> not @@ Env.mem e program.p_vars) program.p_eqs);

  let env' = Env.of_list program.p_eqs in
  Option.fold ~none:()
    ~some:(fun (name, _) -> Format.eprintf "Variable \"%s\" does not have a definition@." name; exit 1)
    (Env.find_first_opt (fun e -> not @@ (Env.mem e env' || List.mem e program.p_inputs )) program.p_vars);

  let check_var id =
    if not @@ Env.mem id program.p_vars then
      Format.eprintf "Referenced variable \"%s\" has not been found among declared variables@." id
  in
  let assert_same_size value observed_size lhs expected_size =
    if observed_size <> expected_size then begin
      Format.eprintf "In \"%s\"'s definition, value \"%s\" has size %d but was expected to have size %d@." lhs value observed_size expected_size;
      exit 1
    end
  in

  let rec check_eq_vars = function
    | Erom (_, _, _, Avar id)  | Eslice (_, _, Avar id) | Eselect (_, Avar id)
    | Earg Avar id | Ereg id | Enot Avar id -> check_var id
    | Ebinop (_, id1, id2) -> List.iter check_eq_vars [Enot id1; Enot id2]
    | Econcat (id1, id2) ->
        List.iter check_eq_vars [Enot id1; Enot id2]
    | Emux (id0, id1, id2) -> List.iter check_eq_vars [Enot id0; Enot id1; Enot id2]
    | Eram (_, _, _, id1, id2, id3, id4) -> List.iter check_eq_vars [Enot id1; Enot id2; Enot id3; Enot id4]
    | _ -> ()
  in

  let check_eq_size lhs = function
    | Ereg id ->
        assert_same_size id (get_size (Avar id)) lhs (get_size (Avar lhs))
    | Earg a  | Enot a ->
        assert_same_size (Format.asprintf "%a" print_var a) (get_size a) lhs (get_size (Avar lhs))
    | Ebinop (_, a1, a2) ->
        assert_same_size (Format.asprintf "%a" print_var a1) (get_size a1) lhs (get_size (Avar lhs));
        assert_same_size (Format.asprintf "%a" print_var a2) (get_size a2) lhs (get_size (Avar lhs))
    | Econcat (a1, a2) ->
        let value = Format.asprintf "CONCAT %a %a" print_var a1 print_var a2 in
        assert_same_size value (get_size a1 + get_size a2) lhs (get_size (Avar lhs))
    | Emux (a0, a1, a2) ->
        assert_same_size (Format.asprintf "%a" print_var a0) (get_size a0) lhs 1;
        assert_same_size (Format.asprintf "%a" print_var a1) (get_size a1) lhs (get_size (Avar lhs));
        assert_same_size (Format.asprintf "%a" print_var a2) (get_size a2) lhs (get_size (Avar lhs))
    | Eslice (i, j, a) ->
        let n = get_size a in
        if (j - i + 1 < 1) || i < 0 || j >= n then begin
          Format.eprintf "In \"%s\"'s definition, SLICE indexes invalid, [%d, %d] not sub [0, %d-1] or empty@." lhs i j n;
        exit 1 end;
        assert_same_size (Format.asprintf "%a" print_var a) (j - i + 1) lhs (get_size (Avar lhs))
    | Eselect (i, a) ->
        let n = get_size a in
        if i < 0 || i >= n then begin
          Format.eprintf "In \"%s\"'s definition, SELECT index invalid, %d not in [0, %d-1]@." lhs i n;
        exit 1 end;
        assert_same_size (Format.asprintf "%a" print_var a) 1 lhs (get_size (Avar lhs))
    | Eram (addr, ws, _, ra, we, wa, data) ->
        assert_same_size (Format.asprintf "%a" print_var ra) (get_size ra) lhs addr;
        assert_same_size (Format.asprintf "%a" print_var we) (get_size we) lhs 1;
        assert_same_size (Format.asprintf "%a" print_var wa) (get_size wa) lhs addr;
        assert_same_size (Format.asprintf "%a" print_var data) (get_size data) lhs ws;
        if (get_size (Avar lhs)) <> ws then begin
          Format.eprintf "In \"%s\"'s definition, value %d was expected to be %d@." lhs ws (get_size (Avar lhs));
          exit 1
        end
    | Erom (addr, ws, _, ra) ->
        assert_same_size (Format.asprintf "%a" print_var ra) (get_size ra) lhs addr;
        if (get_size (Avar lhs)) <> ws then begin
          Format.eprintf "In \"%s\"'s definition, value %d was expected to be %d@." lhs ws (get_size (Avar lhs));
          exit 1
        end
  in

  List.iter (fun (lhs, eq) -> check_eq_vars eq; check_eq_size lhs eq) program.p_eqs
