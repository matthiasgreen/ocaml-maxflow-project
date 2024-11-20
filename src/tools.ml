open Graph

let clone_nodes: 'a graph -> 'b graph = raise Not_found
let gmap: 'a graph -> ('a -> 'b) -> 'b graph = raise Not_found
let add_arc: int graph -> id -> id -> int -> int graph = raise Not_found