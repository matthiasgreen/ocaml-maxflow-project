open Graph

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