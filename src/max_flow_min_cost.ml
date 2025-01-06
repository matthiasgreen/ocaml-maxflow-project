open Graph
open Tools
open Short_path_find

let get_max_flow_min_cost capa_cost_gr src tgt =
  let add_reverse_edges gr = 
    e_fold gr (
      fun new_graph {src; tgt; lbl=(_, cost)} -> new_arc new_graph {src=tgt; tgt=src; lbl=(0, -cost)}
    )
  in
  let increase_flow residual_gr path =
    e_fold residual_gr (
      fun new_graph {src; tgt; lbl=(resid, cost)} ->
        let path_arc = List.find_opt (fun arc -> arc.src = src && arc.tgt = tgt) path in
        match path_arc with
        | None -> new_graph
        | Some path_arc -> add_arc_cost 
    )
  in
  let rec loop residual_gr =
    let path = get_short_path (fun resid -> resid > 0) residual_gr src tgt in
    match path with
    | None -> residual_gr
    | Some (cost, path) ->
      (* Modify residual path, then call again *)
      loop
  in
  let res = loop (add_reverse_edges capa_cost_gr) in
  res
;;
