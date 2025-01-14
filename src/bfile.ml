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

let bp_to_file path (bp:(string * (string * int) list, string) bipartite_problem) bp_res =

  let outfile = open_out path in

  (* print resources and their use *)
  let rec print_bp_resources = function
    | [] -> ()
    | (v_name, v_quantity)::sl ->
      let res_v_names_list = List.map (fun ((_, _), v_name) -> v_name) bp_res in
      let v_name_list = List.filter (fun res_v_name -> res_v_name = v_name) res_v_names_list in
      let v_usage = List.length v_name_list in
      Printf.fprintf outfile "%s: %d/%d\n" v_name v_usage v_quantity;
      print_bp_resources sl
  in

  (* print candidates and the usage of their request(s) *)
  let rec print_bp_candidates = function
    | [] -> ()
    | ((u_name, _), u_quantity)::sl ->
      let res_u_names_list = List.map (fun ((u_name, _), _) -> u_name) bp_res in
      let u_name_list = List.filter (fun res_u_name -> res_u_name = u_name) res_u_names_list in
      let u_usage = List.length u_name_list in
      Printf.fprintf outfile "%s: %d/%d\n" u_name u_usage u_quantity;
      print_bp_candidates sl
  in

  let rec print_res = function
    | [] -> ()
    | ((u_name, _), v_name) :: rest ->
        Printf.fprintf outfile "%s -> %s\n" u_name v_name;
        print_res rest
  in
  
  Printf.fprintf outfile "\tResources:\n";
  print_bp_resources bp.v;

  Printf.fprintf outfile "\n\tCandidates:\n";
  print_bp_candidates bp.u;

  Printf.fprintf outfile "\n\tResult:\n";
  print_res bp_res;
  
  close_out outfile
