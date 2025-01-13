type ('a, 'b) bipartite_problem = {
  u: ('a * int) list;
  v: ('b * int) list;
  map: 'a -> 'b -> int option
}

val solve_bipartite: ('a, 'b) bipartite_problem -> ('a * 'b) list
