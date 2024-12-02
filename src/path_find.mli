open Graph

(*
  Returns the shortest path from source node to target node where each arc satisfies pred.
  Args: pred, graph, source, target
  Returns: the list of arcs that make up the path
*)
val get_path: ('a -> bool) -> 'a graph -> id -> id -> 'a arc list option
