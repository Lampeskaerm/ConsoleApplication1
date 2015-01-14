module ASTToTree

open AST
open Program

let rec exp e =
    match e with
    | Int x -> Node ("Int " + (string x), [])
    | Bool b -> Node ("Bool " + (string b), [])
    | String s -> Node ("String " + s, [])
    | Var x -> Node ("Var " + (string x), []) 
    | ContOf x -> Node ("ContOf", [exp x]) 
    | Apply (n,xs) -> Node ("Apply", [exp (String n)] @ List.fold (fun acc x -> (exp x)::acc) [] xs)
    | Length s -> Node("Length", [exp (String s)])
    | ArrayExp (s,ex) -> Node("ArrayExp", [exp (String s);exp ex])

and stm st =
    match st with
    | Asg (ex1, ex2) -> Node ("Asg", [exp ex1; exp ex2])
    | PrintLn e -> Node ("PrintLn", [exp e])
    | PrintStm s -> Node ("PrintStm", [stm s])
    | Seq ls -> Node ("Seq", List.fold (fun acc s -> (stm s)::acc) [] ls)
    | While (e,s) -> Node ("While", [exp e; stm s])
    | Block (ld,s) -> Node ("Block", (List.fold (fun acc d -> (dec d)::acc) [] ld) @ [stm s])
    | Call (s,le) -> Node ("Call", [exp (String s)] @ List.fold (fun acc e -> (exp e)::acc) [] le)
    | Return e -> Node ("Return", [exp e])
    | If (e,s1,s2) -> Node ("If", [exp e; stm s1; stm s2])
    | If1 (e,s1) -> Node ("If1", [exp e; stm s1])
    | Read e -> Node ("Read", [exp e])
    | Write e -> Node ("Write", List.fold (fun acc x -> (exp x)::acc) [] e)

and dec d =
    match d with
    | VarDec (s,e) -> Node ("VarDec", [exp (String s); exp e])
    | ProcDec (s,ls,st) -> Node ("ProcDec", [exp (String s)] @ (List.fold(fun acc x -> (exp (String x))::acc) [] ls) @ [stm st])
    | RecDec de -> Node ("RecDec", [dec de])
    | ArrayDec (s,e1,e2) -> Node ("ArrayDec", [exp (String s);exp e1; exp e2])