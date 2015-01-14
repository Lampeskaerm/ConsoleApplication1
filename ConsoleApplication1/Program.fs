
module Program

open AST

type 'a Tree = Node of 'a * ('a Tree list)
type Extent = (float*float) list

let fontsize = 20.0;;
let maxWidth = 350.0*0.6;;

let movetree (Node((label, x), subtrees), x' : float) = Node((label, x+x'), subtrees);;

let moveextent (e : Extent, x) = List.map (fun (p,q) -> (p+x,q+x)) e;;

let rec merge = function
    | ([],qs) -> qs
    | (ps,[]) -> ps
    | ((p,_)::ps, (_,q)::qs) -> (p,q)::merge(ps,qs);;

let mergelist es = List.foldBack(fun acc s -> merge (acc,s)) es []

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

//Aux function for getTreePosition
let rec getTreePosition' = function
    | [] -> 0.0
    | ((_,y)::re) -> let value = getTreePosition' re  
                     if y < value then y else value

//Gets the float of the leftmost position in tree
let getTreePosition tree = 
    let re = snd (design' tree)
    getTreePosition' re;;                       

//Adjusts the treeposition so the leftmost position will be placed at point 10
let adjustTreePosition tree = let pos = (getTreePosition tree)
                              if pos > 10.0 then movetree ((design tree),(pos)) else movetree ((design tree),-(-10.0+pos));;

//Tests

let a = Node("A", []);;
let b = Node("B", []);;
let c = Node("C", [a;b]);;
let d = Node("D", [a;b;c]);;
let d1 = Node ("D", [d;b;c]);;
let e = Node ("E", [d1;d;a;c]);;

let designtest1 = design c;;
let designtest2 = design d;;
let designtest3 = design a;;

//let gtptest1 = getTreePosition d1;;

//let atptest0 = adjustTreePosition d;;
//let atptest1 = adjustTreePosition d1;;
let atptest2 = adjustTreePosition e;;
