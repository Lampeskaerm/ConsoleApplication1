module ToPS

open Program
open System.IO

let moveToParent (ppos,vpos) = (string ppos) + " " + (string (vpos+fontsize-2.0)) + " moveto \n"

let moveToCharStart (ppos,vpos) = (string (ppos)) + " " + (string (vpos-fontsize)) + " moveto \n"

let rec listToString (ppos,vpos) = function
    | [] -> ""
    | x::xs -> listToString (ppos,vpos) xs + treeToString (ppos,vpos) x

and treeToString (ppos,vpos) = function
    | Node ((a, hpos:float),[]) -> moveToParent (ppos+(maxWidth/2.0),vpos) + (string (ppos+hpos+(maxWidth/2.0))) + " " + (string (vpos)) + " lineto \n" + moveToCharStart ((ppos+(hpos)), vpos) + "(" + (sprintf "%A" a) + ") show \n"
    | Node ((a, hpos:float),xs) -> moveToParent (ppos+(maxWidth/2.0),vpos) + (string (ppos+hpos+(maxWidth/2.0))) + " " + (string (vpos)) + " lineto \n" + moveToCharStart ((ppos+(hpos)), vpos) + "(" + (sprintf "%A" a) + ") show \n" + listToString (ppos+hpos,(vpos-fontsize*2.0)) xs;; 

let callStringTree initpv = function
    | Node ((a, initph),xs) -> moveToParent (initph,initpv+2.0) + "(" + (sprintf "%A" a) + ") show \n" + listToString (initph, (initpv)) xs;;

let rec multiplyTree (Node((n,p),l)) = Node((n,p*(maxWidth)), List.map (fun x -> (multiplyTree x)) l);; 

let printToFile f s = File.WriteAllText (f,s);;

let preString = "%!\n1 1 scale\n/Courier\n10 selectfont\nnewpath\n"

let endString = "stroke\nshowpage"

System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__;;
let createFinalTree tree = let atp = adjustTreePosition tree
                           let mt = multiplyTree atp
                           let finalString = preString + (callStringTree (800.0) mt) + endString
                           printToFile "output.ps" finalString;;

//Tests

let bum = multiplyTree atptest2;;

//let ttstest1 = treeToString (0.0,0.0) (designtest3);;
//let ttstest2 = treeToString (0.0,0.0) (designtest1);;
//let ttstest3 = callStringTree (10.0) (atptest0);;

let ttstest4 = callStringTree (800.0) (bum);;
//let ttstest5 = callStringTree (800.0) (atptest2);;

