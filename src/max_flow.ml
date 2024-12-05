open Graph
open Tools
open Path_find

(*
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
*)

(* Reverse a given edge i.e. src becomes tgt and tgt bocomes src *)
let get_reversed_edge edge =
  {src=edge.tgt ; lbl=edge.lbl ; tgt=edge.src}

(* Tell if two edges have the same location i.e. src and tgt are the same *)
let same_edge_location edge other_edge =
  edge.src = other_edge.src &&
  edge.tgt = other_edge.tgt

(* Remove a value from every edge of a path in a graph *)
let decrease_on_path graph path value =
  (* Decrease the input edge depending if it's in the given path or not *)
  let decrease_edge graph_edge =
    (* Tell if an edge is the same as the given graph edge *)
    let same_edge = same_edge_location graph_edge in
    (* If the given graph edge is a part of the given path *)
    if List.exists (fun path_edge -> same_edge path_edge) path then
      {src=graph_edge.src ; lbl=graph_edge.lbl - value ; tgt=graph_edge.tgt}
    else {src=graph_edge.src ; lbl=graph_edge.lbl ; tgt=graph_edge.tgt}
  in
  gmap_arcs graph decrease_edge
;;

let get_max_flow capacity_gr src tgt =
  (* Create the flow graph (tell the current found flow for each edges) *)
  let flow_graph =
    gmap capacity_gr (fun _ -> 0)
  and
  (* Create the residual graph (contains the backward flow as well) *)
  residual_graph =
    gmap capacity_gr (fun x -> x)
  in

  (* Main loop *)
  let rec ford_fulkerson fgr rgr =
    (* Get a path fro src to tgt in the residual graph *)
    let path_finding_result =
      get_path (fun a -> a > 0) rgr src tgt
    in

    if path_finding_result = None then gmap fgr (fun x -> (x, x))
    else
    
    let augmenting_path = Option.get path_finding_result in

    (* Get the augmenting value *)
    let augmenting_value =
      List.fold_left (fun min arc2 -> if min < arc2.lbl then min else arc2.lbl) Int.max_int augmenting_path
    in

    (* Remove the augmenting value from the flow graph *)
    let new_flow_graph = decrease_on_path fgr augmenting_path augmenting_value in

    (* Remove the augmenting value from the flow graph *)
    let diminished_rgr = decrease_on_path rgr augmenting_path augmenting_value in

    (* Add the augmenting path backwards to the residual graph *)
    let new_residual_graph =
      List.fold_left (fun acu path_edge -> new_arc acu (get_reversed_edge path_edge)) diminished_rgr augmenting_path
    in

    ford_fulkerson new_flow_graph new_residual_graph

  in
  ford_fulkerson flow_graph residual_graph

;;

  (* legacy 

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
;;*)

let get_max_flow_number gr src =
  List.fold_left (fun total {lbl=(f, _); _} -> total + f) 0 (out_arcs gr src)
;;
