
#load "AST.fs"
#load "Program.fs"
#load "ASTToTree.fs"

open AST
open Program
open ASTToTree


//Tests
let test1 = createFinalTree(stm(Block
                                    ([VarDec ("rng",Int 100); VarDec ("len",Int 10);
                                      ArrayDec ("a",ContOf (Var "len"),Int 1); VarDec ("sum",Int 0)],
                                     Seq
                                       [Asg (Var "a",Apply ("randomArray",[Var "rng"; Var "len"]));
                                        Call ("printArray",[ContOf (Var "a")]);
                                        Asg (Var "sum",Apply ("sumArray",[ContOf (Var "a")]));
                                        PrintLn (String "Sum:");
                                        PrintLn (Apply ("toString",[ContOf (Var "sum")]))])));;

let test2 = 
     createFinalTree(stm(Block
                            ([VarDec ("x",Int 4); VarDec ("output",Int 1);
                              RecDec
                                (ProcDec
                                   ("fac",["n"; "o"],
                                    If1
                                      (Apply ("<>",[ContOf (Var "n"); Int 0]),
                                       Seq
                                         [Asg
                                            (Var "o",Apply ("*",[ContOf (Var "n"); ContOf (Var "o")]));
                                          Asg (Var "n",Apply ("-",[ContOf (Var "n"); Int 1]));
                                          Call ("fac",[Var "n"; Var "o"])])))],
                             Seq
                               [Call ("fac",[Var "x"; Var "output"]);
                                PrintLn (Apply ("toString",[ContOf (Var "x")]));
                                PrintLn (Apply ("toString",[ContOf (Var "output")]))])));;