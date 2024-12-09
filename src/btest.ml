open Bipartite
open Bfile

let rec print_res = function
| [] -> print_endline "done"
| ((u_name, _), v_name) :: rest -> 
  Printf.printf "%s -> %s\n" u_name v_name;
  print_res rest


let () =
  let infile = Sys.argv.(1)
  and _outfile = Sys.argv.(2) in

  let bp = bp_from_file infile in
  let res = solve_bipartite bp in

  print_res res;

  ()
