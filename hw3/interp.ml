open Ast
open Ds

(* I pledge my honor that I have abided by the Stevens Honor System
-Daniel Kramer *)

let rec eval (en:env) (e:expr):exp_val =
  match e with
  | Int n           -> NumVal n
  | Var x           -> lookup en x
  | Let(x, e1, e2)  ->
    let v1 = eval en e1  in
    eval (extend_env en x v1) e2
  | IsZero(e1)      ->
    let v1 = eval en e1  in
    let n1 = numVal_to_num v1 in
    BoolVal (n1 = 0)
  | ITE(e1, e2, e3) ->
    let v1 = eval en e1  in
    let b1 = boolVal_to_bool v1 in
    if b1 then eval en e2 else eval en e3
  | Sub(e1, e2)     ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) - (numVal_to_num v2))
  | Add(e1, e2)     ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) + (numVal_to_num v2))
  | Div(e1, e2)     ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) / (numVal_to_num v2))
  | Mul(e1, e2)     ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) * (numVal_to_num v2))
  | Abs(e1)         -> 
    let v1 = eval en e1 in
    (match v1 with
    | _ when (numVal_to_num v1) < 0 -> NumVal ( 0 - (numVal_to_num v1))
    | _ -> NumVal (numVal_to_num v1))
  | Cons(e1, e2)    -> 
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    ListVal (v1 :: (listVal_to_list v2))
  | Hd(e1)          -> 
    let v1 = eval en e1 in 
    (match v1 with
    | ListVal([]) -> failwith("List is empty!")
    | ListVal(x::xs) -> x
    | _ -> failwith("error"))
  | Tl(e1)          ->
    let v1 = eval en e1 in
    (match v1 with
    | ListVal([]) -> failwith("List is empty!")
    | ListVal(x::xs) -> ListVal(xs)
    | _ -> failwith("error"))
  | Empty(e1)       -> 
     let v1 = eval en e1 in
     (match v1 with
      | ListVal(x) -> BoolVal(x = [])
      | TreeVal(x) -> BoolVal(x = Empty)
      | _ -> failwith("Not List or Tree")
      (* supports both lists and trees *))
  | EmptyList       -> ListVal([])
  | EmptyTree       -> TreeVal(Empty)
  | Node(e1,lt,rt)  ->
    let va = eval en e1 in
    let vl = eval en lt in
    let vr = eval en rt in
    (match vl,vr with
    | TreeVal(_),TreeVal(_) -> TreeVal(Node(va, treeVal_to_tree vl, treeVal_to_tree vr))
    | _ -> failwith("Expecting left and right trees"))
  | CaseT(target,emptycase,id_e,id_lt,id_rt,nodecase) -> 
    let v1 = eval en target in
    (match treeVal_to_tree v1 with
      | Empty -> eval en emptycase
      | Node(ev1, ev2, ev3) -> 
        let env1 = extend_env en id_e ev1 in
        let env2 = extend_env env1 id_lt (TreeVal(ev2)) in
        let env3 = extend_env env2 id_rt (TreeVal(ev3)) in
        eval env3 nodecase)
  
(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let interp (e:string):exp_val =
  e |> parse |> eval (empty_env ())
