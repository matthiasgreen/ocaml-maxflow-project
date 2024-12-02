open Graph
open Tools
open Path_find

let print_path path =
  print_endline "Path:";
  List.iter (fun arc -> Printf.printf "%d -> %d (%d)\n" arc.src arc.tgt arc.lbl) path;
  print_endline ""
;;

let print_graph graph =
  print_endline "Graph:";
  e_iter graph (fun arc -> Printf.printf "%d -> %d (%d)\n" arc.src arc.tgt arc.lbl);
  print_endline ""
;;

let get_max_flow capacity_gr src tgt =
  (* Set flow to 0 for all edges *)
  let init_gr = gmap capacity_gr (fun _ -> 0) in

  (* Main loop *)
  let rec loop gr =
    (* Create graph with remaining capacity as labels *)
    let remaining_gr = better_gmap
      gr (fun arc -> {arc with lbl=((Option.get (find_arc capacity_gr arc.src arc.tgt)).lbl - arc.lbl)})
    in
    (* Get augmenting path from source to target *)
    let augmenting_path = get_path (fun x -> x > 0) remaining_gr src tgt in

    (* Get the max flow along the path *)
    let get_max_path_flow path =
      List.fold_left (fun min arc2 -> if min < arc2.lbl then min else arc2.lbl) Int.max_int path
    in
    match augmenting_path with
    | None -> gr
    | Some path ->
      print_endline "Augmenting path found: ";
      print_path path;
      let min_path_flow = get_max_path_flow path in
      Printf.printf "Min path flow: %d\n" min_path_flow;
      (* Add max flow to all arcs in the path and recurse *)
      let arc_in_path arc =
        match List.find_opt (fun arc2 -> arc2.src = arc.src && arc2.tgt = arc.tgt) path with
        | None -> false
        | Some _ -> true
      in
      let next_graph = (better_gmap gr (fun arc -> {arc with lbl=(
        if arc_in_path arc then arc.lbl + min_path_flow else arc.lbl
      )})) in
      print_graph next_graph;
      loop next_graph
  in
  let max_flow_graph = loop init_gr in
  better_gmap max_flow_graph (fun arc -> {arc with lbl=(
    (arc.lbl, (Option.get (find_arc capacity_gr arc.src arc.tgt)).lbl)
  )})
;;