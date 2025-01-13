open Graph

(*
  Returns the shortest path from source node to target node where each arc satisfies pred.
  Args: pred, graph, source, target
  Returns: the list of arcs that make up the path
*)
val get_path: ('a -> bool) -> 'a graph -> id -> id -> 'a arc list option

(* 
Args:
  pred: a predicate which determines whether or not a node should be visited
  gr: ('a * int) graph, int should be the cost
  src
  tgt
Returns:
  (int, ('a * id) arc list) option
  None if no augmenting path found
  Some (total_cost, arcs) if augmenting path found
*)
val get_short_path:
  ('a -> bool) ->
  ('a * int) graph ->
  id ->
  id ->
  (int * ('a * int) arc list) option