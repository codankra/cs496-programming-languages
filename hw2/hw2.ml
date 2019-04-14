(* I pledge my honor that I have abided by the Stevens HOnor System - Daniek Kramer *)

(*--*)
type dTree = Node of char*dTree*dTree | Leaf of int;;

(* Personal Example for Part 3 *)
let myExample = Node('a',Leaf(4),Node('b',Leaf(1),Leaf(2)));;

(*--*)
let tLeft = Node('w',Node('x',Leaf(2),Leaf(5)),Leaf(8));;
let tRight = Node('w',Node('x',Leaf(2),Leaf(5)),Node('y',Leaf(7),Leaf(5)));;

(* Sample encoding provided in assignment pdf *)
let pEncoding = (['x';'y';'z'],[([0;0;0],0);([0;0;1],1);([0;1;0],1);([0;1;1],0);([1;0;0],1);([1;0;1],0);([1;1;0],0);([1;1;1],1)])

(*--*)
let rec dTree_height: dTree -> int = function
| Leaf(_) -> 0
| Node(_,x,y) -> 1 + max (dTree_height x) (dTree_height y)

(*--*)
let rec dTree_size: dTree -> int = function
| Leaf(_) -> 1
| Node(_,x,y) -> 1 + (dTree_size x) + (dTree_size y)

(*-Helper function for dTree_paths-*)
let rec consAll: int -> int list list -> int list list = fun a l ->
match a,l with
 | a,x::[] -> [a::x]
 | a,[] -> [[a]]
 | a,x::xs -> (a :: x) :: (consAll a xs)
 (*Takes int and list and makes int first of all lists *)

(*--*)
let rec dTree_paths: dTree -> int list list = function
| Leaf(_) -> []
| Node(_,x,y) -> (consAll 0 (dTree_paths x)) @ (consAll 1 (dTree_paths y))


(*--*)
let rec dTree_is_perfect: dTree -> bool = function
| Leaf(_) -> true
| Node(_,x,y) -> (dTree_height x) = (dTree_height y) 
&& (dTree_is_perfect x) && (dTree_is_perfect y)
(* dTree perfect if hieghts are equal at every split *)

(*--*)
let rec dTree_map: (char -> char) -> (int -> int) -> dTree -> dTree = fun f g t ->
match t with
| Leaf(a) -> Leaf(g a)
| Node(a,x,y) -> Node(f a, dTree_map f g x, dTree_map f g y)

(*--*)
let rec list_to_tree: char list -> dTree = function
| [] -> Leaf(0)
| x::xs -> Node(x, list_to_tree xs, list_to_tree xs)


(* - Helper function for replace_leaf_at - *)
let rec applyValues: dTree -> (int list * int) -> dTree = fun t f ->
match f,t with
| ([],a),Leaf(_) -> Leaf(a)
| ([],a),Node(_,_,_) -> failwith "error: replace function applied to node"
| (0::xs,a),Node(b,c,d) -> Node(b,(applyValues c (xs,a)),d)
| (1::xs,a),Node(b,c,d) -> Node(b,c,(applyValues d (xs,a)))
| f,t -> failwith "didn't match any case"


(* assumes graph of function is a list of int 2-tuples
 * where first val is input and second val is output *)
let rec replace_leaf_at: dTree -> (int list * int) list -> dTree = fun t x ->
match t,x with
| t,[] -> t
| t,x::xs -> replace_leaf_at (applyValues t x) xs


(* Pair-Encoding: (char list * (int list * int) list) *)
let bf_to_dTree: (char list * (int list * int) list) -> dTree = function
| (k,x) -> replace_leaf_at (list_to_tree k) x 