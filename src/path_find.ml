open Graph
open Tools

module S = Set.Make (struct type t = id let compare = Stdlib.compare end)

(* Update the hashtable with current cost.
   Returns true if the program should continue searching this branch, otherwise false. *)
let visit_node hashtable node prev_node cost =
  (* If node has already been visited *)
  if Hashtbl.mem hashtable node
  then (
    (* If current cost if higher or equal than previous cost, return false *)
    if (let (prev_cost, _) = Hashtbl.find hashtable node in prev_cost <= cost)
    then false
    (* If current cost is lower, update min cost and return true *)
    else (Hashtbl.replace hashtable node (cost, prev_node); true)
  )
  (* Add cost and return true *)
  else (Hashtbl.add hashtable node (cost, prev_node); true)
;;

(* Iterate over nodes in the graph and update cost table with (min_cost, prev_node) *)
let fill_cost_table pred cost_table gr src tgt =
  let visit = visit_node cost_table in

  let rec loop curr_node curr_cost prev_node =
    if visit curr_node prev_node curr_cost then
      (* Stop current branch if we have reached the target *) 
      if curr_node != tgt then 

        (* Call loop on all nodes accessible from current node *)
        let rec inner_loop = function
          | [] -> ()
          | arc :: rest ->
            let {lbl=(label, cost); _} = arc in
            (* Don't explore if pred if false *)
            if not (pred label) then inner_loop rest else
              (* Explore all children of this node, then continue *)
              loop arc.tgt (curr_cost + cost) (Some curr_node);
            inner_loop rest
        in
        inner_loop (out_arcs gr curr_node)
  in
  loop src 0 None;
  cost_table
;;

(* In order to get the shortest path from cost_map, we need to iterate backwards from the target, following the prev_node we put in the cost_map *)
let get_path_from_table cost_table gr tgt =
  (* Iterate from target to source, following prev_node of cost map *)
  let rec reverse_iter node =
    match Hashtbl.find cost_table node with
    | (_, None) -> []
    | (_, Some next) -> match find_arc gr next node with | None -> failwith "Failed to find path" | Some path -> path :: reverse_iter next
  in
  reverse_iter tgt

let get_short_path pred gr src tgt =

  (* This map stores the min cost to reach the node and optionally the node which it was reached from *)
  let cost_table = fill_cost_table pred (Hashtbl.create (n_nodes gr)) gr src tgt in

  if Hashtbl.mem cost_table tgt then
    let (cost, _) = Hashtbl.find cost_table tgt in
    Some (cost, get_path_from_table cost_table gr tgt)
  else None
;;


(*
  First implementation: DFS
  Innefficient but simple.
*)
let get_path pred gr src tgt =
  (* Store visited nodes with Set reference *)
  let visited_nodes = ref S.empty in
  let is_visited el = S.mem el !visited_nodes in
  let visit el = visited_nodes := S.add el !visited_nodes; in

  (* Recurse through graph *)
  let rec loop current_node =
    (* If the current node is visited, then skip, else visit *)
    if is_visited current_node then None else (
      visit current_node;
      (* If current node is the target, return an empty path *)
      if current_node = tgt then Some [] else

        (* 
          Otherwise, call self on children of current node
          Find the first result which is not None and append visited arc
        *)
        let rec inner_loop = function
          | [] -> None
          | arc :: rest ->
            if not (pred arc.lbl) then inner_loop rest else 
              let result = loop arc.tgt in
              match result with
              | None -> inner_loop rest
              | Some path -> Some (arc :: path)
        in
        inner_loop (out_arcs gr current_node)
    )
  in loop src
;;