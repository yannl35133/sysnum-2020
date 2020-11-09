exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n :: g.g_nodes

let node_of_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  try
    let n1 = node_of_label g id1 in
    let n2 = node_of_label g id2 in
    n1.n_link_to   <- n2 :: n1.n_link_to;
    n2.n_linked_by <- n1 :: n2.n_linked_by
  with Not_found -> Format.eprintf "Tried to add an edge between non-existing nodes"; raise Not_found

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g =
  clear_marks g;
  let rec dfs n =
    if n.n_mark = InProgress then raise Cycle;
    
    if n.n_mark = NotVisited then begin
      n.n_mark <- InProgress;
      List.iter dfs n.n_link_to;
      n.n_mark <- Visited
    end
  in
  try begin
    List.iter dfs g.g_nodes; false
  end
  with Cycle -> true

let topological g =
  clear_marks g;
  let rec dfs acc n =
    if n.n_mark = InProgress then raise Cycle;
    
    if n.n_mark = NotVisited then begin
      n.n_mark <- InProgress;
      let res = n.n_label :: List.fold_left dfs acc n.n_link_to in
      n.n_mark <- Visited;
      res
    end
    else acc
  in
  List.fold_left dfs [] g.g_nodes

  