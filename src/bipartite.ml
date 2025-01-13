open Graph
open Max_flow_min_cost

type ('a, 'b) bipartite_problem = {
  u: ('a * int) list;
  v: ('b * int) list;
  map: 'a -> 'b -> int option
}

let bipartite_to_graph bp =
  (* First, we need to convert bp to a graph. We also need to store a id -> 'a/'b hashmap to retrieve information at the end *)
  let htu = Hashtbl.create (List.length bp.u) in
  let htv = Hashtbl.create (List.length bp.v) in
  let rec add_nodes ht gr n = function
  | [] -> (gr, n)
  | el :: rest -> 
    Hashtbl.add ht n el; 
    add_nodes ht (new_node gr n) (n+1) rest
  in
  let (u_node_graph, n) = add_nodes htu empty_graph 0 bp.u in
  let (node_graph, n) = add_nodes htv u_node_graph (n+1) bp.v in
  (* We also want to add source and target nodes *)
  let src = n+1 in
  let node_graph = new_node node_graph src in
  let tgt = src+1 in
  let node_graph = new_node node_graph tgt in

  (* 
    Now we can add the arcs:
    iterate through u * v and add arc with weight if map is not None
  *)
  let gr = ref node_graph in
  Hashtbl.iter (
    (fun src (u, _) -> Hashtbl.iter (
      fun tgt (v, _) -> match bp.map u v with | None -> () | Some weight -> gr := new_arc !gr {src; tgt; lbl=(1, weight)}
    ) htv)
  ) htu;
  (* Now add arcs between source and u *)
  Hashtbl.iter (
    fun tgt (_, n) -> gr := new_arc !gr {src; tgt; lbl=(n, 0)}
  ) htu;
  (* Now add arcs between v and tgt*)
  Hashtbl.iter (
    fun src (_, n) -> gr := new_arc !gr {src; tgt; lbl=(n, 0)}
  ) htv;
  (* Now return graph and hts *)
  (!gr, htu, htv, src, tgt)
;;

let solve_bipartite bp =
  let gr, htu, htv, source, target = bipartite_to_graph bp in
  (*write_file "bipartite_graph" (gmap gr string_of_int);*)
  let max_flow_graph = get_max_flow_min_cost gr source target in
  (* Now we need to iterate through u->v edges and see which ones have 1, 1*)
  let res = ref [] in
  e_iter max_flow_graph (
    fun {src; tgt; lbl=(f, _)} -> if f = 1 && src != source && tgt != target then
      let (u_name, _) = Hashtbl.find htu src in
      let (v_name, _) = Hashtbl.find htv tgt in
       res := (u_name, v_name) :: !res
  );
  !res

