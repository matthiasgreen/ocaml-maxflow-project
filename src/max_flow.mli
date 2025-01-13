open Graph

(*
  Returns the max flow from target to source using the Ford-Fulkerson Algorithm.
  Args: flow network, source, target
  Returns: The max flow
*)
val get_max_flow: int graph -> id -> id -> (int * int) graph

(*
  Takes a (capacity, cost) graph, a source and a sink.
  Returns a (flow, capacity) graph with the total flow maximized, in the configuration which minimizes the total cost.
*)
val get_max_flow_min_cost: (int * int) graph -> id -> id -> (int * int) graph

val get_max_flow_number: (int * int) graph -> id -> int
