open Graph

type 'a input_graph = ('a * int) graph

let get_short_path pred gr src tgt =
  (* Create hashtable with size = number of nodes *)
  let cost_map = Hashtbl.create (n_fold gr (fun a _ -> a + 1) 0) in

  let visit n c = 
    (* If node has already been visited *)
    if Hashtbl.mem cost_map n 
    then (
      (* If current cost if higher or equal than previous cost, return false *)
      if (Hashtbl.find cost_map n <= c)
      then false
      (* If current cost is lower, update min cost and return true *)
      else (Hashtbl.replace cost_map n c; true)
    )
    (* Add cost and return true *)
    else (Hashtbl.add cost_map n c; true)
  in
  
  let rec loop curr_node curr_cost =
    (* No need to return cost in recursion since we can get it from hashtable at the end *)
    if visit curr_node curr_cost then 
      if curr_node = tgt then 
        Some []
      else

        (* Call self on children of current node *)
        let rec inner_loop = function
        | [] -> None
        | arc :: rest ->
          let {lbl=(label, cost); _} = arc in
          if not (pred label) then inner_loop rest else
            let result = loop arc.tgt (curr_cost + cost) in
            match result with
            | None -> inner_loop rest
            | Some path -> Some (arc :: path)
          in
        inner_loop (out_arcs gr curr_node)
    else None
  in
  let res = loop src 0 in
  match res with
  | None -> None
  | Some path -> Some (Hashtbl.find cost_map tgt, path)
;;
  
