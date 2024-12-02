open Graph

let clone_nodes graph =
    n_fold graph new_node empty_graph

let gmap graph fm =
    e_fold graph (fun acu arc -> new_arc acu {src=arc.src;tgt=arc.tgt;lbl=fm arc.lbl}) (clone_nodes graph)
    

let add_arc: int graph -> id -> id -> int -> int graph = raise Not_found