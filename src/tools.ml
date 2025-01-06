open Graph

let clone_nodes graph =
    n_fold graph new_node empty_graph

let gmap graph fm =
    e_fold graph (fun acu arc -> new_arc acu {src=arc.src;tgt=arc.tgt;lbl=fm arc.lbl}) (clone_nodes graph)
    

let gmap_arcs graph fm =
    e_fold graph (fun acu arc -> new_arc acu (fm arc)) (clone_nodes graph)


let add_arc gr src tgt n =
    let possible_arc = find_arc gr src tgt in
    match possible_arc with
    | None -> (new_arc gr {src; tgt; lbl=n})
    | Some _ ->
        gmap_arcs gr (
            function
            | a when a.src = src && a.tgt = tgt -> {a with lbl=a.lbl+n} 
            | a -> a
        )
;;

let add_arc_cost gr src tgt n =
    let possible_arc = find_arc gr src tgt in
    match possible_arc with
    | None -> gr
    | Some _ ->
        gmap_arcs gr (
            function
            | {src=a_src; tgt=a_tgt; lbl=(a_val, a_cost)} when a_src = src && a_tgt = tgt -> 
                {src=a_src; tgt=a_tgt; lbl=(a_val+n, a_cost)} 
            | a -> a
        )
;;