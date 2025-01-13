open Graph

(*
  Returns a new graph having the same nodes as the input, but no arcs.
*)
val clone_nodes: 'a graph -> 'b graph

(*
  Maps all arcs of the input graph by a mapping function.
*)
val gmap: 'a graph -> ('a -> 'b) -> 'b graph

val gmap_arcs: 'a graph -> ('a arc -> 'b arc) -> 'b graph

(*
  Adds n to the value of the arc between id1 and id2. If the arc does not exist, it is created.
*)
val add_arc: int graph -> id -> id -> int -> int graph

val add_arc_cost: (int * int) graph -> id -> id -> int -> (int * int) graph

val n_nodes: ('a graph) -> int