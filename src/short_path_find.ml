open Graph


let get_short_path pred gr src tgt =
  (* Create hashtable with size = number of nodes *)
  let cost_map = Hashtbl.create (n_fold gr (fun a _ -> a + 1) 0) in

  let visit n prev_n c = 
    (* If node has already been visited *)
    if Hashtbl.mem cost_map n
    then (
      (* If current cost if higher or equal than previous cost, return false *)
      if (let (cost, _) = Hashtbl.find cost_map n in cost <= c)
      then false
      (* If current cost is lower, update min cost and return true *)
      else (Hashtbl.replace cost_map n (c, prev_n); true)
    )
    (* Add cost and return true *)
    else (Hashtbl.add cost_map n (c, prev_n); true)
  in
  
  let rec loop curr_node curr_cost prev_node =
    (* No need to return cost in recursion since we can get it from hashtable at the end *)
    if visit curr_node prev_node curr_cost then 
      if curr_node != tgt then 

        (* Call self on children of current node *)
        let rec inner_loop = function
        | [] -> ()
        | arc :: rest ->
          let {lbl=(label, cost); _} = arc in
          if not (pred label) then inner_loop rest else
            loop arc.tgt (curr_cost + cost) (Some curr_node);
            inner_loop rest
          in
        inner_loop (out_arcs gr curr_node)
  in
  loop src 0 None;

  if Hashtbl.mem cost_map tgt then
    let (cost, _) = Hashtbl.find cost_map tgt in
    (* Iterate from target to source, following prev_node of cost map *)
    let rec reverse_iter node =
      match Hashtbl.find cost_map node with
        | (_, None) -> []
        | (_, Some next) -> match find_arc gr next node with | None -> failwith "Failed to find path" | Some path -> path :: reverse_iter next
    in
    Some (cost, reverse_iter tgt)
  else None
;;
  
