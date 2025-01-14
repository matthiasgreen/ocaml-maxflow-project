open Graph

(*
  This function is no longer needed, but has been kept to show our work.
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

(*
  Returns the total flow out of the source node (equal to the total flow into the sink node)
  Args: (flow, _) graph (result of get_max_flow or get_max_flow_min_cost)
*)
val get_max_flow_number: (int * int) graph -> id -> int
