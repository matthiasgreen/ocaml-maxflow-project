open Graph

(*
  Returns the max flow from target to source using the Ford-Fulkerson Algorithm.
  Args: flow network, source, target
  Returns: The max flow
*)
val get_max_flow: int graph -> id -> id -> (int * int) graph

val get_max_flow_number: (int * int) graph -> id -> int