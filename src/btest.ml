open Bipartite
open Bfile

let () =
  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 3 then
  begin
    Printf.printf
      "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
      ("    🟄  infile  : input file containing a graph\n" ^
       "    🟄  outfile : output file in which the result should be written.\n\n") ;
    exit 0
  end ;
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2) in

  let bp = bp_from_file infile in
  let res = solve_bipartite bp in

  let () = bp_to_file outfile bp res in

  ()
