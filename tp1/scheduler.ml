open Netlist_ast
open Graph

exception Combinational_cycle

let rec deps_exp = function
  | Earg a ->                                   deps_arg a
  | Enot a ->                                   deps_arg a
  | Ebinop (_, a, b) ->         List.concat_map deps_arg [a; b]
  | Emux (a, b, c) ->           List.concat_map deps_arg [a; b; c]
  | Econcat (a, b) ->           List.concat_map deps_arg [a; b]
  | Eslice (_, _, a) ->                         deps_arg a
  | Eselect (_, a) ->                           deps_arg a
  | Ereg _ ->                                   []
  | Erom (_, _, ra) ->                          deps_arg ra
  | Eram (_, _, ra, _we, _wa, _wi) ->           deps_arg ra


and deps_arg = function
  | Aconst _ -> []
  | Avar id ->  [id]

 
let schedule p =
  let g = mk_graph () in

  List.iter (add_node g) p.p_inputs;
  List.iter (fun (a, _) -> add_node g a) p.p_eqs;

  List.iter (fun (id, exp) -> List.iter (add_edge g id) (deps_exp exp) ) p.p_eqs;

  let rev_ordered_idents =
    try topological g
    with Cycle -> raise Combinational_cycle
  in
  let rev_ordered_eqs : equation list = List.filter_map (fun a -> List.find_opt (fun (a', _) -> a = a') p.p_eqs) rev_ordered_idents in
  { p with p_eqs = List.rev rev_ordered_eqs }
 