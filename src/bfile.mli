open Bipartite

type path = string

val bp_from_file: path -> (string * string list, string) bipartite_problem

