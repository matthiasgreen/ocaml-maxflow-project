open Bipartite

type path = string

type bp = ((string * (string * int) list), string) bipartite_problem

(* let print_bp bp =
   Printf.printf "u:\n";
   List.iter (fun ((name, list), n) -> Printf.printf "%d %s [%s]\n" n name (String.concat ", " list)) bp.u;
   Printf.printf "v:\n";
   List.iter (fun (name, n) -> Printf.printf "%s %d\n" name n) bp.v;
   Printf.printf "map:\n";
   ()
   ;; *)

let add_u (bp: bp) u_name n =
  {bp with u=(
       ((u_name, []), n) :: bp.u
     )}

let add_v bp v n =
  {bp with v=((v, n) :: bp.v)}
;;

let add_e (bp: bp) u_name v_name weight =
  let res = List.find_opt (fun ((name, _), _) -> name = u_name) bp.u in
  match res with
  | None -> failwith "Cannot add edge"
  | Some _ -> {bp with u=(
      List.map (fun ((name, list), n) -> if name = u_name then ((name, (v_name, weight) :: list), n) else ((name, list), n)) bp.u
    )}

let read_u bp line =
  try Scanf.sscanf line "u %s %d %s@%%" 
        (fun name n _ -> add_u bp name n)
  with e ->
    Printf.printf "Cannot read u in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"
;;

let read_v bp line =
  try Scanf.sscanf line "v %s %d %s@%%" (
      fun name n _ -> add_v bp name n
    ) with e ->
    Printf.printf "Cannot read v in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"
;;

let read_e bp line =
  try Scanf.sscanf line "e %s %s %d %s@%%" (
      fun u_name v_name weight _ -> add_e bp u_name v_name weight
    ) with e ->
    Printf.printf "Cannot read e in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"
;;

let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

let bp_from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop bp =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let bp2 =
        (* Ignore empty lines *)
        if line = "" then bp

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          | 'u' -> read_u bp line
          | 'v' -> read_v bp line
          | 'e' -> read_e bp line

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment bp line
      in      
      loop bp2

    with End_of_file -> bp (* Done *)
  in

  let final_graph = loop {u=[]; v=[]; map=(
      fun (_, u_val) v -> match List.find_opt (fun (v_val, _) -> v_val = v) u_val with
        | None -> None
        | Some (_, weight) -> Some weight
    )} in

  close_in infile ;
  (* print_bp final_graph; *)
  final_graph

