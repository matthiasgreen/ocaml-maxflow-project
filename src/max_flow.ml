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

let edge_add_value edge value =
  {src = edge.src ; lbl = edge.lbl + value ; tgt = edge.tgt}

(* Reverse a given edge i.e. src becomes tgt and tgt bocomes src *)
let get_reversed_edge edge =
  {src=edge.tgt ; lbl=edge.lbl ; tgt=edge.src}

let get_reversed_edge_with_value edge value =
  {src=edge.tgt ; lbl=value ; tgt=edge.src}

(* Tell if two edges have the same location i.e. src and tgt are the same *)
let same_edge_location edge other_edge =
  edge.src = other_edge.src &&
  edge.tgt = other_edge.tgt

let is_in_path edge path =
  let is_same_edge =
    same_edge_location edge
  in
  List.exists (fun path_edge -> is_same_edge path_edge) path

let is_counter_flow edge path =
  let is_reversed_edge path_edge =
    same_edge_location edge (get_reversed_edge path_edge)
  in
  List.exists (fun path_edge -> is_reversed_edge path_edge) path

let augment_residual_graph residual_graph augmenting_path augmenting_value =
  let augment_edge residual_edge =
    if is_in_path residual_edge augmenting_path
    then edge_add_value residual_edge (-augmenting_value)
    else if is_counter_flow residual_edge augmenting_path
      then edge_add_value residual_edge augmenting_value
      else residual_edge
    in
  gmap_arcs residual_graph augment_edge

let get_max_flow capacity_gr src tgt =
  (* Function used at the end to rebuild the flow graph from the residual graph *)
  let get_flow_graph_from_residual_graph residual_graph =
    let get_flow_edge capacity_edge =
      match find_arc residual_graph capacity_edge.src capacity_edge.tgt with
      | None -> {src = capacity_edge.src ; lbl = (capacity_edge.lbl, capacity_edge.lbl) ; tgt = capacity_edge.tgt}
      | Some residual_edge -> {src = residual_edge.src ; lbl = (residual_edge.lbl, capacity_edge.lbl) ; tgt = residual_edge.tgt}
    in
    gmap_arcs capacity_gr get_flow_edge
  in

  (* Create the residual graph (contains the backward flow as well) *)
  let residual_graph =
    gmap capacity_gr (fun x -> x)
  in

  (* Main loop *)
  let rec ford_fulkerson rgr =
    (* Get a path fro src to tgt in the residual graph *)
    let path_finding_result =
      get_path (fun a -> a > 0) rgr src tgt
    in

    if path_finding_result = None then get_flow_graph_from_residual_graph rgr
    else
    
    let augmenting_path = Option.get path_finding_result in

    (* Get the augmenting value *)
    let augmenting_value =
      List.fold_left (fun min arc2 -> if min < arc2.lbl then min else arc2.lbl) Int.max_int augmenting_path
    in

    (* Add the opposite augmenting value to a counter flow edge *)
    let augmented_rgr =
      augment_residual_graph rgr augmenting_path augmenting_value
    in

    let new_residual_graph =
      List.fold_left (fun acu path_edge -> match find_arc residual_graph path_edge.tgt path_edge.src with
      | None -> new_arc acu (get_reversed_edge_with_value path_edge augmenting_value)
      | Some _ -> acu) augmented_rgr augmenting_path
    in

    ford_fulkerson new_residual_graph

  in
  ford_fulkerson residual_graph

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
