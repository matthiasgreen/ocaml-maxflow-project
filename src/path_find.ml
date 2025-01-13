open Graph

module S = Set.Make (struct type t = id let compare = Stdlib.compare end)

(*
  First implementation: DFS
  Innefficient but easier to do recursively.
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
