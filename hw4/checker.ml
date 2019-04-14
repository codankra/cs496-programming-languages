open Ast

(* I pledge my honor that I have abided by the Stevens Honor System
- DANIEL KRAMER
10426217 *)

let from_some = function
  | None -> failwith "from_some: None"
  | Some v -> v

(*  ;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;; *)

type tenv =
  | EmptyTEnv
  | ExtendTEnv of string*texpr*tenv

let empty_tenv () = EmptyTEnv

let extend_tenv id t tenv = ExtendTEnv(id,t,tenv)


let rec apply_tenv (tenv:tenv) (id:string):texpr option =
  match tenv with
  | EmptyTEnv -> None
  | ExtendTEnv (key,value,tenv1) ->
    if id=key
    then Some value
    else apply_tenv tenv1 id


let init_tenv () =
     extend_tenv "x"  IntType
     @@ extend_tenv "v" IntType
     @@ extend_tenv "i"  IntType
     @@ empty_tenv ()

let rec  string_of_tenv  = function
  | EmptyTEnv -> ""
  | ExtendTEnv(id,v,env) -> "("^id^","^string_of_texpr v^")"^string_of_tenv env



let rec type_of_prog = function
  | AProg e -> type_of_expr (init_tenv ()) e
and
  type_of_expr en = function
  | Int n          -> IntType
  | Var id          ->
    (match apply_tenv en id with
    | None -> failwith @@ "Variable "^id^" undefined"
    | Some texp -> texp)
  | Unit -> UnitType
  | ITE(e1, e2, e3)    ->
    let t1 = type_of_expr en e1
    in let t2 = type_of_expr en e2
    in let t3 = type_of_expr en e3
    in if t1=BoolType && t2=t3
    then t2
    else failwith "ITE: Type error"
  | Add(e1, e2) | Mul(e1,e2) | Sub(e1,e2) | Div(e1,e2)    ->
    let t1 = type_of_expr en e1 in
    let t2 = type_of_expr en e2  in
    if t1=IntType && t2=IntType
    then IntType
    else failwith "Add: arguments must be ints"
  | IsZero(e) ->
    let t1 = type_of_expr en e  in
    if t1=IntType
    then BoolType
    else failwith "Zero?: argument must be int"
  | Let(x, e1, e2) ->
    let t1 = type_of_expr en e1
    in type_of_expr (extend_tenv x t1 en) e2
  | Proc(x,ty,e)      ->
    let tc= type_of_expr (extend_tenv x ty en) e
    in FuncType(ty,tc)
  | App(e1,e2)     ->
    let t1 = type_of_expr en e1
    in let t2 = type_of_expr en e2
    in (match t1 with
    | FuncType(td,tcd) when td=t2 -> tcd
    | FuncType(td,tcd) -> failwith "App: argument does not have correct type"
    | _ -> failwith "Checker: App: LHS must be function type")
  | Letrec(tRes,id,param,tParam,body,e) ->
    let t=type_of_expr (extend_tenv param tParam
                          (extend_tenv id (FuncType(tParam,tRes)) en))
        body
    in if t=tRes
    then type_of_expr (extend_tenv id (FuncType(tParam,tRes)) en) e
    else failwith
        "Checker: LetRec: Types of recursive function does not match declaration"
  | Set(id,e) ->
      failwith "EXPLICIT-REFS: Set not a valid operation"
  | BeginEnd(es) ->
    List.fold_left (fun v e -> type_of_expr en e) UnitType es

  (* explicit ref *)
  | NewRef(e) ->
    let t = type_of_expr en e
    in RefType(t)
  | DeRef(e) -> let t = type_of_expr en e
    in (match t with
    | RefType x -> x
    | _ -> failwith "Cannot dereference e")
  | SetRef(e1,e2) -> let t1 = type_of_expr en e1 in
    let t2 = type_of_expr en e2 in (match t1 with
      | RefType x -> if x = t2 then UnitType else failwith "Type of argument does not match reference!"
      | _ -> failwith "Expected a reference!")

  (* pair *)
  | Pair(e1, e2) ->
    let t1 = type_of_expr en e1
    in let t2 = type_of_expr en e2
    in PairType(t1, t2)
  | Unpair(id1, id2, def, body) ->
    let tdef = type_of_expr en def in
    (match tdef with
      | PairType(a,b) -> let env1 = extend_tenv id1 a en in 
      let env2 = extend_tenv id2 b env1 in type_of_expr env2 body
      | _ -> failwith "Expected a PairVal")
    (* let env1 = extend_tenv en id1 in  *)

  (* list *)
  | EmptyList(t) ->
    ListType t
  | Cons(he, te) ->
    let the = type_of_expr en he in
    let tte = type_of_expr en te in
    (match tte with
      | ListType a -> if a = the then tte else failwith "cannot cons list of two different types"
      | _ -> failwith "Hd: Expected a list!")
  | Null(e) ->
    (match type_of_expr en e with
      | ListType _ -> BoolType
      | _ -> failwith "Null: Expression given is not a list!")
  | Hd(e) ->
    let t = type_of_expr en e in
    (match t with
      | ListType a -> a
      | _ -> failwith "Hd: Expected a list!")
  | Tl(e) ->
    let t = type_of_expr en e in
    (match t with
      | ListType a -> t
      | _ -> failwith "Tl: Expected a list!")

  (* tree *)
  | EmptyTree(t) ->
    TreeType t
  | Node(de, le, re) ->
    let tde = type_of_expr en de in 
    let tle = type_of_expr en le in
    let tre = type_of_expr en re in 
    (match tle,tre with
      | TreeType a, TreeType b -> if a = b && a = tde then TreeType tde
        else failwith "Node: Inconsistent type in tree"
      | _,_ -> failwith "Node: Expexted a tree!")
  | NullT(t) ->
    (match type_of_expr en t with
      | TreeType _ -> BoolType
      | _ -> failwith "NullT: Expression given is not a tree!")
  | GetData(t) ->
    (match type_of_expr en t with
      | TreeType a -> a
      | _ -> failwith "GetData: Expression given is not a tree!")
  | GetLST(t) ->
    (match type_of_expr en t with
      | TreeType a -> TreeType a
      | _ -> failwith "GetLST: Expression given is not a tree!")
  | GetRST(t) ->
    (match type_of_expr en t with
      | TreeType a -> TreeType a
      | _ -> failwith "GetRST: Expression given is not a tree!")


  | Debug ->
    print_string "Environment:\n";
    print_string @@ string_of_tenv en;
    UnitType



let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let chk (e:string) : texpr =
  e |> parse |> type_of_prog

