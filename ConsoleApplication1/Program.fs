
type 'a Tree = Node of 'a * ('a Tree list)
type Extent = (float*float) list

let movetree (Node((label, x), subtrees), x' : float) = Node((label, x+x'), subtrees);;

let moveextent (e : Extent, x) = List.map (fun (p,q) -> (p+x,q+x)) e;;

let rec merge = function
    | ([],qs) -> qs
    | (ps,[]) -> ps
    | ((p,_)::ps, (_,q)::qs) -> (p,q)::merge(ps,qs);;

let mergelist es = List.fold(fun acc x -> merge ( x, acc)) [] es;;

let rmax (p:float, q:float) = if p > q then p else q;;

let rec fit xs ys = 
    match (xs,ys) with
    | (((_,p)::ps),((q,_)::qs)) -> rmax(fit ps qs, p - q + 1.0)
    | _ -> 0.0
    
let rec fitlistl' acc es = 
    match (acc,es) with
    | (_,[]) -> []
    | (acc, e::es) -> let x = fit acc e
                      x::fitlistl' (merge (acc, moveextent (e,x))) es;;

let fitlistl es = fitlistl' [] es;;

let rec fitlistr' acc es =
    match (acc,es) with
    | (_,[]) -> []
    | (acc, e::es) -> let x = -(fit e acc)
                      x::fitlistr' (merge (moveextent (e,x),acc)) es;;

let fitlistr es = List.rev (fitlistr' [] (List.rev es));;

let mean (x,y) = (x+y)/2.0;;

let fitlist es = List.map mean (List.zip (fitlistl es) (fitlistr es));;

let rec design' (Node(label, subtrees)) = 
    let (trees,extents) = List.unzip (List.map design' subtrees)
    let positions = fitlist extents
    let ptrees = List.map movetree (List.zip (trees) (positions))
    let pextents = List.map moveextent (List.zip (extents) (positions))
    let resultextent = (0.0, 0.0) :: mergelist pextents
    let resulttree = Node((label, 0.0), ptrees)
    (resulttree, resultextent);;

let design tree = fst (design' tree);;

//Tests

let a = Node("A", []);;
let b = Node("B", []);;
let c = Node("C", [a;b]);;

