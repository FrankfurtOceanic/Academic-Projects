(* TODO: Write some tests for neighbours. Consider creating a graph first,
 and then writing your tests based on it *)

(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* We've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let neighbours_tests: ((string graph * string) * (string * weight) list) list = [
  
  
  (({nodes = ["a"; "b";"c"]; 
     edges = [("a", "b",1);("a", "c",1)]}, "a"),
   [("b", 1); ("c", 1)]);
  (({nodes = ["a"; "b";"c"]; 
     edges = [("a", "b",1);("a", "c",1)]}, "a"),
   [("c", 1);("b", 1)]);
  
  (({nodes = ["a"; "b";"c"]; 
     edges = [("a", "b",1);("a", "c",1)]}, "c"),
   []);
  (({nodes = ["a"; "b";"c"]; 
     edges = [("c", "a",1)]}, "c"),
   [("a",1)]);
  
  
  
]

(* TODO: Implement neighbours. *)
let neighbours (g: 'a graph) (vertex: 'a) : ('a * weight) list = 
  let f = (fun (v1,v2,w) acc -> 
      if v1 = vertex then (v2,w)::acc
      else acc 
    )in
  List.fold_right f g.edges []

(* TODO: Implement find_path. *)
let find_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  
  let add = (fun (l,wt) (v,w) -> (v::l, wt+w) )in
  
  
  let rec aux_node (v,w) (visited : 'a list) : ('a list * weight) =  (*given a node and the current weight, if it hasn't been visited return (neighbours, node.weight + weight)*)
    if (List.exists (fun vis -> vis = v ) visited) then raise Fail  
    else if (v=b) then  ([],0)
    else aux_list (neighbours g v) (v::visited) 
      
      

  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) = match nodes with 
    |[] -> raise Fail
    |(v,w)::t -> try (add (aux_node (v,w) visited)  (v,w)) with Fail -> aux_list t (v::visited) 
  in
  
  aux_list [(a, 0)] []

(* TODO: Implement find_path'. *)
let find_path' (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node (v,w) (visited : 'a list) fc sc : ('a list * weight)=
    if (List.exists (fun vis -> vis = v ) visited) then fc () 
    else if (v=b) then  sc ([],0)
    else aux_list (neighbours g v) (v::visited) fc sc
      
      
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) fc sc : ('a list * weight) = match nodes with
    |[] -> fc ()
    |(v,w)::t -> aux_node (v,w) visited (fun () -> aux_list t (v::visited) fc sc) (fun (l,wt) -> sc (v::l, w+wt) )  
  
  in
  
  aux_list [(a,0)] [] (fun () -> raise Fail) (fun x -> x)


(* TODO: Implement find_all_paths *)
let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list =
  let rec aux_node (v,w) (visited : 'a list)  : ('a list * weight) list =
    if (List.exists (fun vis -> vis = v ) visited) then raise Fail 
    else if (v=b) then [([],0)]
    else aux_list (neighbours g v) (v::visited) 
    
        

      
      
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) list = match nodes with
    |[] -> raise Fail
    |(v,w)::t -> 
        
        try ( (List.map (fun (l2,w2) -> (v::l2, w+w2)) (aux_node (v,w) (visited))) @ (try aux_list t (visited) with Fail -> [])) with Fail-> 
          (aux_list t (v::visited)) 

  
  
  in
  try (aux_list [(a,0)] [] ) with Fail -> []
    


(* TODO: Implement find_shortest_path *)
let find_shortest_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) option =
  match (find_all_paths g a b) with 
  |[] -> None
  |x::t -> Some (List.fold_left (fun (l1, w1) (l2, w2) -> if (w2<w1) then (l2, w2)
                                  else (l1,w1)) x t)

(* ---------- Hamming Numbers ----------- *)

let rec merge (s1: int str) (s2: int str) : int str =
  let sh1 = s1.hd in
  let sh2 = s2.hd in
  if(sh1 < sh2) then
    
    {hd = sh1 ; 
     tl = Susp (fun () -> merge (force(s1.tl)) s2 )
    }
  else if(sh2 < sh1) then
    {hd = sh2 ; 
     tl = Susp (fun () -> merge s1 (force(s2.tl)) )
    }
  else (*sh1 = sh2*)
    {hd = sh2 ; 
     tl = Susp (fun () -> merge (force(s1.tl)) (force(s2.tl)) )
    }
    

let rec hamming_series =
  {hd = 1;
   tl = Susp (fun ()-> merge (merge (times 2 hamming_series) (times 3 hamming_series)) (times 5 hamming_series) )}
