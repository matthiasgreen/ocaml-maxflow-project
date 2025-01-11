open Graph
open Short_path_find
open Tools

(* Input type, capa * cost *)
type capacity_cost_gr = (int * int) graph;;

(* 
  A residual graph also contains the cost for pathfinding purposes
  (residual flow * cost)
*)
type residual_graph = (int * int) graph;;

let print_path path =
  print_endline "Path:";
  List.iter (fun {src; tgt; lbl=(capa, cost)} -> Printf.printf "%d -> %d (%d %d)\n" src tgt capa cost) path;
  print_endline ""
;;

let print_graph graph =
  print_endline "Graph:";
  e_iter graph (fun {src; tgt; lbl=(capa, cost)} -> Printf.printf "%d -> %d (%d %d)\n" src tgt capa cost);
  print_endline ""
;;

let get_max_flow_min_cost (gr: capacity_cost_gr) src tgt =
  (* Add reverse edges to a (capacity, cost) graph ((0, -cost))*)
  let add_reverse_edges (gr: capacity_cost_gr) = 
    e_fold gr (
      fun new_graph {src; tgt; lbl=(_, cost)} -> new_arc new_graph {src=tgt; tgt=src; lbl=(0, -cost)}
    ) gr
  in

  let flow_along_path path = 
    List.fold_left (
      fun min_flow {lbl=(flow, _); _} -> Int.min min_flow flow  
    ) Int.max_int path
  in

  let is_in_path path f_src f_tgt =
    match List.find_opt (fun {src; tgt; _} -> src=f_src && tgt=f_tgt) path with 
    | None -> false
    | Some _ -> true
  in

  (* 
    Takes a residual graph and a path.
    Increases the flow (forward and backwards) as much as possible.
  *)
  let increase_flow (gr: residual_graph) path =
    let flow = flow_along_path path in
    e_fold gr (
      (* If edge not in path, add edge unmodifed*)
      (* elif edge in path, add edge with decreased residual flow *)
      (* elif backwards edge in path, add edge with increased residual flow *)
      fun new_gr {src; tgt; lbl=(f, cost)} -> 
        if is_in_path path src tgt then new_arc new_gr {src; tgt; lbl=(f-flow, cost)} else
          if is_in_path path tgt src then new_arc new_gr {src; tgt; lbl=(f+flow, cost)} else
            new_arc new_gr {src; tgt; lbl=(f, cost)}
        
    ) (clone_nodes gr)
  in

  (* White short path exists, increase flow *)
  let rec loop residual_gr =
    let path = get_short_path (fun resid -> resid > 0) residual_gr src tgt in
    match path with
    | None -> residual_gr
    | Some (cost, path) ->
      print_int cost;
      print_newline ();
      print_path path;
      print_int (flow_along_path path);
      print_newline ();
      print_newline ();
      (* Modify residual path, then call again *)
      loop (increase_flow residual_gr path)
  in

  let gr = add_reverse_edges gr in

  loop gr
  
;;

