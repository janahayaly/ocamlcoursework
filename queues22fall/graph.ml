(******************************************************************************)
(* Challenge Problem: Graphs **************************************************)
(******************************************************************************)

(* This is a challenge problem, and it is worth no points.  It is fairly
   difficult, since it exercises almost everything we've taught you so
   far.  If you have time to at least take a crack at it, we highly
   recommend doing so!

   Our topic is directed graphs.  When computer scientists talk about
   graphs, they usually don't mean bar graphs or other visual ways of
   displaying tables of data.  Instead they tend to mean a
   network-like diagram, consisting of NODES connected by EDGES.

   In our graphs, we'll be generic in the "name" labels used to
   identify nodes: we only ever need to compare labels for equality,
   so it shouldn't matter what type they are.

   We'll model directed graphs, where the presence of an edge from n1
   to n2 does not imply the presence of an edge from n2 to n1.

   Some of the code below uses the OCaml list module.  You can see
   this module's documentation at:

   http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html *)

;; open Assert

type 'a node  = { name: 'a;
                  edges: 'a list ref }

type 'a graph = 'a node list ref

(* Here's a graph:

   /-----------\
   |           |
   V           |
   a---->b--+->d--\
   ^     |  |  ^  |
   |     V  |  |  |
   \---->c--/  \--/

 The nodes of this graph are a, b, c, and d.  The edges are depicted
 as lines; arrow heads indicate direction.

 Notice that the line between a and c on the lower-left has two
 arrows: a links to c and c links to a.

 Here are the edges:

   a links to b and c,
   b links to c and d,
   c links to a and d, and  (the c to d link is hard to draw in ascii!)
   d links to a and d.

 Here's how we represent that graph in our data structure: *)

let g : string graph =
  ref [{name="a";edges=ref ["b";"c"]};
       {name="b";edges=ref ["c";"d"]};
       {name="c";edges=ref ["a";"d"]};
       {name="d";edges=ref ["a";"d"]}]

(* Returns true if the graph contains a node with the given name. *)
let contains_node (name:'a) (g:'a graph) : bool =
  List.exists (fun (n:'a node) -> n.name = name) !g

(* Finds a node in a graph. *)
let lookup (name:'a) (g:'a graph) : 'a node =
  List.find (fun (n:'a node) -> n.name = name) !g

(* Adds a new node to the graph. *)
let add (name:'a) (g:'a graph) : unit =
  g := {name=name;edges=ref []}::!g

(* We define some functions which determine whether a graph is
   'valid', i.e. it satisfies these GRAPH INVARIANTS
       - Every node has a unique name
       - Every edge in a node's edge list is actually in the graph *)

let rec valid_edges (es:'a list) (seen:'a list) : 'a list =
  List.filter (fun (e:'a) -> not (List.mem e seen)) es

let rec valid_nodes (ns:'a node list) (seen:'a list) (need:'a list) : bool =
  begin match ns with
    | [] -> need = []
    | n::ns ->
        (not (List.mem n.name seen)) &&
          let need' = valid_edges !(n.edges) (seen@need) in
          let need = List.filter
            (fun (name:'a) -> name <> n.name) (need'@need)
          in
            valid_nodes ns (n.name::seen) need
  end

let valid (g:'a graph) : bool =
  valid_nodes !g [] []

(* Now you: Fill in below in the places indicated... *)

let invalid_graph () : string graph =
  failwith "invalid_graph: unimplemented"

let test () : bool =
  valid g
;; run_test "valid g" test

let test () : bool =
  not (valid (invalid_graph ()))
;; run_test "not valid invalid_graph" test

(* Write a function that adds an edge from the node named src to the
   node named tgt.

   Make sure you return a valid graph! *)

let link (src:'a) (tgt:'a) (g:'a graph) : unit =
  failwith "link: unimplemented"

(* Write a function that removes a node from a graph.  Make sure you
   return a valid graph! *)

let remove (name:'a) (g:'a graph) : unit =
  failwith "remove: unimplemented"

let test () : bool =
  let g : string graph =
     ref [{name="a";edges=ref ["b";"c"]};
          {name="b";edges=ref ["c";"d"]};
          {name="c";edges=ref ["a";"d"]};
          {name="d";edges=ref ["a";"d"]}] in
  remove "a" g;

  [{name="b";edges=ref ["c";"d"]};
   {name="c";edges=ref ["d"]};
   {name="d";edges=ref ["d"]}] = !g
;; run_test "remove a" test


(* Finally, please finish our implementation of "depth-first search".

   DFS works like so:

   (1) Pop a node n off the stack.  If the stack is empty, then the
       seen list contains the nodes visited in reverse order.
   (2) If n has been seen, go to (1).
   (3) If not, put all of the nodes that n links to on the stack.
   (4) Add n to the seen list, and go to (1). *)

let rec dfs_aux (stack:'a node list) (seen:'a list) (g:'a graph) : 'a list =
failwith "dfs_aux: unimplemented"

let dfs (g:'a graph) : 'a list =
  begin match !g with
    | [] -> []
    | n::_ -> dfs_aux [n] [] g
  end

let test () : bool =
  let g : string graph =
     ref [{name="a";edges=ref ["b";"c"]};
          {name="b";edges=ref ["c";"d"]};
          {name="c";edges=ref ["a";"d"]};
          {name="d";edges=ref ["a";"d"]}] in
  ["a";"b";"c";"d"] = dfs g
;; run_test "dfs g (1)" test

let test () : bool =
 let g : string graph =
     ref [{name="b";edges=ref ["c";"d"]};
          {name="a";edges=ref ["b";"c"]};
          {name="c";edges=ref ["a";"d"]};
          {name="d";edges=ref ["a";"d"]}] in
  ["b";"c";"a";"d"] = dfs g
;; run_test "dfs g (2)" test
