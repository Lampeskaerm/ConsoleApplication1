module ToPS

open Program
open System.IO


System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__;;


let rec calcMaxWidthList n = function
    | [] -> n
    | x::xs -> if calcMaxWidthList n xs > calcMaxWidth n x then calcMaxWidthList n xs else calcMaxWidth n x

and calcMaxWidth n = function
    | Node(a,[]) -> if (String.length (sprintf "%A" a)) > n then (String.length (sprintf "%A" a)) else n
    | Node(a,l::ln) -> let w = if (String.length (sprintf "%A" a)) > n then (String.length (sprintf "%A" a)) else n
                       calcMaxWidth (calcMaxWidthList w ln) l;;
                   
//let rec calcMaxWidth (Node (a,l)) = List.fold (fun acc (Node(a1,l)) -> if (String.length (sprintf "%A" a1)) > acc then (String.length (sprintf "%A" a1)) else acc) 0 l;;

let moveToParent (ppos,vpos) = (string ppos) + " " + (string (vpos+fontsize-2.0)) + " moveto \n";;

let calculateCharPos mw pos a = let diff = (mw - (fontsize*0.6*(float (String.length (sprintf "%A" a)))))
                                if diff <= 0.0 then (string pos) else (string (pos+(diff/2.0)));;

let moveToCharStart mw (pos,vpos) a = (calculateCharPos mw pos a) + " " + (string (vpos-fontsize)) + " moveto \n"

let initialMove mw (pos,vpos) a = (string (calculateCharPos mw pos a)) + " " + (string (vpos+fontsize-2.0)) + " moveto \n"

let rec listToString mw (ppos,vpos) = function
    | [] -> ""
    | x::xs -> listToString mw (ppos,vpos) xs + treeToString mw (ppos,vpos) x

and treeToString mw (ppos,vpos) = function
    | Node ((a, hpos:float),[]) -> moveToParent (ppos+(mw/2.0),vpos) + (string (ppos+hpos+(mw/2.0))) + " " + (string (vpos)) + " lineto \n" + moveToCharStart mw ((ppos+(hpos)), vpos) a + "(" + (sprintf "%A" a) + ") show \n"
    | Node ((a, hpos:float),xs) -> moveToParent (ppos+(mw/2.0),vpos) + (string (ppos+hpos+(mw/2.0))) + " " + (string (vpos)) + " lineto \n" + moveToCharStart mw ((ppos+(hpos)), vpos) a + "(" + (sprintf "%A" a) + ") show \n" + listToString mw (ppos+hpos,(vpos-fontsize*2.0)) xs;; 

let callStringTree mw initpv = function
    | Node ((a, initph),xs) -> initialMove mw (initph,initpv+2.0) a + "(" + (sprintf "%A" a) + ") show \n" + listToString mw (initph, (initpv)) xs;;

let rec multiplyTree mw (Node((n,p),l)) = Node((n,p*(mw)), List.map (fun x -> (multiplyTree mw x)) l);; 

let printToFile f s = File.WriteAllText (f,s);;

let preString = "%!\n1 1 scale\n/Courier\n" + string fontsize + " selectfont\nnewpath\n";;

let endString = "stroke\nshowpage";;

let createFinalTree tree = let maxWidth = (float (calcMaxWidth 0 tree))*fontsize*0.6
                           let atp = adjustTreePosition tree
                           let mt = multiplyTree maxWidth atp
                           let finalString = preString + (callStringTree maxWidth (800.0) mt) + endString
                           printToFile "output.ps" finalString
                           finalString;;

//Tests

//let bum = multiplyTree atptest2;;

//let ttstest1 = treeToString (0.0,0.0) (designtest3);;
//let ttstest2 = treeToString (0.0,0.0) (designtest1);;
//let ttstest3 = callStringTree (10.0) (atptest0);;

//let ttstest4 = callStringTree (800.0) (bum);;
//let ttstest5 = callStringTree (800.0) (atptest2);;

;;

