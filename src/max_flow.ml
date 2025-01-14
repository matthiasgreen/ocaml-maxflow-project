open Graph
open Tools
open Path_find

(* Add reverse edges to a (residual_flow: int, cost: int) graph.
  For each edge, add an edge with (0, -cost) *)
let add_reverse_edges gr = 
  e_fold gr (
    fun new_graph {src; tgt; lbl=(_, cost)} -> new_arc new_graph {src=tgt; tgt=src; lbl=(0, -cost)}
  ) gr
;;

(* Calculate the maximum flow along a path
  It is equal to the minimum residual flow *)
let flow_along_path path =
  List.fold_left (
    fun min_flow {lbl=(flow, _); _} -> Int.min min_flow flow  
  ) Int.max_int path
;;

(* Check if the arc {f_src; f_tgt} is in the path *)
let is_in_path path f_src f_tgt =
  match List.find_opt (fun {src; tgt; _} -> src=f_src && tgt=f_tgt) path with 
  | None -> false
  | Some _ -> true
;;

(* 
  Takes a residual graph (of type (residual_flow: int, cost: int) graph) and a path.
  Modifies the graph to increase the flow (forward and backwards) along the path.
*)
let increase_flow gr path =
  let flow = flow_along_path path in
  e_fold gr (
    (* If edge not in path, add edge unmodifed *)
    (* elif edge in path, add edge with decreased residual flow *)
    (* elif backwards edge in path, add edge with increased residual flow *)
    fun new_gr {src; tgt; lbl=(f, cost)} -> 
      if is_in_path path src tgt then new_arc new_gr {src; tgt; lbl=(f-flow, cost)} else
      if is_in_path path tgt src then new_arc new_gr {src; tgt; lbl=(f+flow, cost)} else
        new_arc new_gr {src; tgt; lbl=(f, cost)}

  ) (clone_nodes gr)
;;

let get_max_flow_min_cost gr src tgt =

  (* While an augmenting path exists (lowest cost first), increase the flow along that path *)
  let rec loop residual_gr =
    let path = get_short_path (fun resid -> resid > 0) residual_gr src tgt in
    match path with
    | None -> residual_gr
    | Some (_, path) ->
      (* Modify residual path, then call again *)
      loop (increase_flow residual_gr path)
  in

  let rev_gr = add_reverse_edges gr in

  let residual_gr = loop rev_gr in

  (* Convert (residual, cost) graph to (flow, capacity) graph. *)

  e_fold residual_gr (
    (* find edge in original gr and add *)
    fun new_graph {src; tgt; lbl=(resid, _)} -> match find_arc gr src tgt with
      | None -> new_graph (* If this is a backward edge, do not add *)
      | Some {lbl=(capa, _); _} -> new_arc new_graph {src; tgt; lbl=(capa-resid, capa)} 
  ) (clone_nodes residual_gr)
;;


(* Get the total flow out of the source node (equal to the total flow into the sink node) *)
let get_max_flow_number gr src =
  List.fold_left (fun total {lbl=(f, _); _} -> total + f) 0 (out_arcs gr src)
;;


(*
  ===========================================================================
  The following has been kept to show our work.
  It is no longer necessary since you can use max_flow_min_cost with cost = 1
*)

(*  *)
let edge_add_value edge value =
  {src = edge.src ; lbl = edge.lbl + value ; tgt = edge.tgt}

(* Reverse a given edge i.e. src becomes tgt and tgt becomes src *)
let get_reversed_edge edge =
  {src=edge.tgt ; lbl=edge.lbl ; tgt=edge.src}

(* Tell if two edges have the same location i.e. src and tgt are the same *)
let same_edge_location edge other_edge =
  edge.src = other_edge.src &&
  edge.tgt = other_edge.tgt

(* Add a counter flow with 0 value (do not modify already existing counter flow edges) *)
let add_counter_flow graph path =
  List.fold_left (fun acu path_edge ->
      match find_arc graph path_edge.tgt path_edge.src with
      | None -> add_arc acu path_edge.tgt path_edge.src 0 (* add counter flow *)
      | Some _ -> acu (* do nothing if there already is a counter flow *)
    ) graph path

(* Increase flow along path in graph *)
let augment_on_path graph path value =
  let is_in_path edge =
    let is_same_edge =
      same_edge_location edge
    in
    List.exists (fun path_edge -> is_same_edge path_edge) path
  in
  let is_counter_flow edge =
    let is_reversed_edge path_edge =
      same_edge_location edge (get_reversed_edge path_edge)
    in
    List.exists (fun path_edge -> is_reversed_edge path_edge) path
  in
  gmap_arcs graph (fun graph_edge ->
      if is_in_path graph_edge
      then edge_add_value graph_edge (-value)
      else if is_counter_flow graph_edge
      then edge_add_value graph_edge value
      else graph_edge
    )

(* Convert a residual graph in a flow graph using a capacity graph *)
let get_flow_graph capacity_graph residual_graph =
  e_fold residual_graph (fun acu residual_edge ->
      match find_arc capacity_graph residual_edge.src residual_edge.tgt with
      | None -> acu (* discard edges that are not in the capacity edge *)
      | Some capacity_edge ->
        let flow = capacity_edge.lbl - residual_edge.lbl in
        if flow < 0
        then acu (* discard edges that don't represent a positive the flow *)
        else new_arc acu {src = residual_edge.src ; lbl = (flow, capacity_edge.lbl) ; tgt = residual_edge.tgt}
    ) (clone_nodes capacity_graph)

let get_max_flow capacity_gr src tgt =

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

    if path_finding_result = None then get_flow_graph capacity_gr rgr
    else

      let augmenting_path = Option.get path_finding_result in

      (* Get the augmenting value *)
      let augmenting_value =
        List.fold_left (fun min arc2 -> if min < arc2.lbl then min else arc2.lbl) Int.max_int augmenting_path
      in

      (* Add the opposite augmenting value to a counter flow edge *)
      let rgr_with_counter_flow =
        add_counter_flow rgr augmenting_path
      in

      (* Augment the residual graph *)
      let new_residual_graph =
        augment_on_path rgr_with_counter_flow augmenting_path augmenting_value
      in

      ford_fulkerson new_residual_graph

  in
  ford_fulkerson residual_graph

;;
