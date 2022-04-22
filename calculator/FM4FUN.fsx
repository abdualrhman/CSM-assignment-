// This script implements our interactive calculator

// ARTHUR'S COPY PASTE CODE:
// mono "/Users/arthur/Desktop/DTU/02141 Computer Science Modelling/PA/FsLexYacc.10.0.0/build/fslex/net46/fslex.exe" FM4FUNLexer.fsl --unicode
// mono "/Users/arthur/Desktop/DTU/02141 Computer Science Modelling/PA/FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe" FM4FUNParser.fsp --module FM4FUNParser
// fsharpi FM4FUN.fsx

// MARK'S COPY PASTE CODE
// mono /Users/maqixin/FsLexYacc.10.0.0/build/fslex/net46/fslex.exe FM4FUNLexer.fsl --unicode
// mono /Users/maqixin/FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe FM4FUNParser.fsp --module FM4FUNParser
// fsharpi FM4FUN.fsx

// We need to import a couple of modules, including the generated lexer and parser
#r "/Users/arthur/Desktop/DTU/02141 Computer Science Modelling/PA/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System
//open System.Net
//open System.IO

#load "FM4FUNTypesAST.fs"
open FM4FUNTypesAST
#load "FM4FUNParser.fs"
open FM4FUNParser
#load "FM4FUNLexer.fs"
open FM4FUNLexer

/////////////////
//PARSER (TASK 1)

let rec printA e =
    match e with
    | Num (x) -> string x
    | TimesExpr (x, y) -> "TIMES(" + printA (x) + "," + printA (y) + ")"
    | DivExpr (x, y) -> "DIV(" + printA (x) + "," + printA (y) + ")"
    | PlusExpr (x, y) -> "PLUS(" + printA (x) + "," + printA (y) + ")"
    | MinusExpr (x, y) -> "MINUS(" + printA (x) + "," + printA (y) + ")"
    | PowExpr (x, y) -> "POWER(" + printA (x) + "," + printA (y) + ")"
    | UPlusExpr (x) -> "UPLUS(" + printA (x) + ")"
    | UMinusExpr (x) -> "UMINUS(" + printA (x) + ")"
    | Var (x) -> x: string
    | Array (x, y) -> "ARRAY(" + string x + "," + printA (y) + ")"

let rec printB bool =
    match bool with
    | True -> "true"
    | False -> "false"
    // check Formal method syntax
    | AndExpr (x, y) -> "AND(" + printB (x) + "," + printB (y) + ")"
    | AndAndExpr (x, y) -> "ANDAND(" + printB (x) + "," + printB (y) + ")"
    | OrExpr (x, y) -> "OR(" + printB (x) + "," + printB (y) + ")"
    | OrOrExpr (x, y) -> "OROR(" + printB (x) + "," + printB (y) + ")"
    | NotExpr (x) -> "NOT(" + printB (x) + ")"
    | EqualExpr (x, y) -> "EQUAL(" + printA (x) + "," + printA (y) + ")"
    | NotEqualExpr (x, y) -> "NOTEQUAL(" + printA (x) + "," + printA (y) + ")"
    | GreaterExpr (x, y) -> "GT(" + printA (x) + "," + printA (y) + ")"
    | GreaterEqualExpr (x, y) -> "GTE(" + printA (x) + "," + printA (y) + ")"
    | LessExpr (x, y) -> "LT(" + printA (x) + "," + printA (y) + ")"
    | LessEqualExpr (x, y) -> "LTE(" + printA (x) + "," + printA (y) + ")"

let rec printC (command: Command) =
    match command with
    | AssignExpr (x, y) -> "ASSIGNVAR(" + printA (x) + "," + printA (y) + ")"
    | ArrayAssignExpr (x, y) ->"ASSIGNARRAY(" + printA (x) + ","+ printA (y) + ")"
    | SkipExpr -> "SKIP"
    | SEMIExpr (x, y) -> "SEMICOLON(" + printC (x) + "," + printC (y) + ")"
    | IfExpr (x) -> "IF(" + printGC (x) + ")"
    | DoExpr (x) -> "DO(" + printGC (x) + ")"

and printGC (guardedCommand: GC) =
    match guardedCommand with
    | FunGCExpr (x, y) -> "FUNGC(" + printB (x) + "," + printC (y) + ")"
    | ElseIfExpr (x, y) -> "FUNGC(" + printGC (x) + "," + printGC (y) + ")"

/////////////////////////////////
//GENERATE PROGRAM GRAPH (TASK 2)
let mutable nodeID = 1

let fresh () =
    let curNode = nodeID
    nodeID <- curNode + 1
    Node(curNode)

let rec edgesC start finish c =
    match c with
    | AssignExpr (x, a) -> [ C(start, AssignExpr(x, a), finish) ]
    | SkipExpr -> [ C(start, SkipExpr, finish) ]
    | SEMIExpr (x, y) ->
                        let q = fresh ()
                        let E1 = edgesC start q x
                        let E2 = edgesC q finish y
                        E1 @ E2
    | IfExpr (x) -> edgesGC start finish x
    | DoExpr (x) ->
                    let b = doneF x
                    let E = edgesGC start start x 
                    (E @ [ GC(start, b, finish) ])
    | _ -> []

and edgesGC start finish gc =
    match gc with
    | FunGCExpr (x, y) ->
                        let q = fresh ()
                        let E = edgesC q finish y
                        [ GC(start, x, q) ] @ E
    | ElseIfExpr (x, y) ->
                        let E1 = edgesGC start finish x
                        let E2 = edgesGC start finish y
                        E1 @ E2

and doneF a =
    match a with
    | FunGCExpr (x, y) -> (NotExpr(x))
    | ElseIfExpr (x, y) -> AndExpr(doneF x, doneF y)

let rec DEdgesC start finish c=
    match c with
    | AssignExpr (x, a) -> [ C(start, AssignExpr(x, a), finish) ]
    | SkipExpr -> [ C(start, SkipExpr, finish) ]
    | SEMIExpr (x, y) ->
                        let q = fresh ()
                        let E1 = DEdgesC start q x 
                        let E2 = DEdgesC q finish y 
                        E1 @ E2
    | IfExpr (x) ->
                    let (E, d) = DEdgesGC start finish x False
                    E
    | DoExpr (x) ->
                    let (E, d) = DEdgesGC start start x False
                    E @ [ GC(start, NotExpr(d), finish) ]
    | _ -> []

and DEdgesGC start finish gc d =
    match gc with
    | FunGCExpr (x, y) ->
                    let q = fresh ()
                    let E = edgesC q finish y
                    ([ GC(start, AndExpr(x, NotExpr(d)), q) ] @ E), OrExpr(x, d)
    | ElseIfExpr (x, y) ->
                    let (E1, d1) = DEdgesGC start finish x d
                    let (E2, d2) = DEdgesGC start finish y d1
                    (E1 @ E2, d2)

let rec convToGraph (lst: Graph) =
    match lst with
    | [] -> ""
    | C (a, b, c) :: rest ->
            match (a, c) with
            | (Node (x), Node (y)) -> " q" + string x+ " -> q"+ string y + " [label = \""+ string b+ "\"] \n"+ convToGraph rest
    | GC (a, b, c) :: rest ->
            match (a, c) with
            | (Node (x), Node (y)) ->" q"+ string x+ " -> q"+ string y+ " [label = \""+ string b+ "\"] \n"+ convToGraph rest

//////////////////////
//INTERPRETER (TASK 3)

let rec evalB bool mem =
    match bool with
    | True -> true
    | False -> false
    // check Formal method syntax
    | AndExpr (x, y) ->
        if evalB x mem then
            evalB y mem
        else
            false
    | AndAndExpr (x, y) -> evalB x mem && evalB y mem
    | OrExpr (x, y) ->
        if not (evalB x mem) then
            evalB y mem
        else
            true
    | OrOrExpr (x, y) -> evalB x mem || evalB y mem
    | NotExpr (x) -> not (evalB x mem)
    | EqualExpr (x, y) -> evalA x mem = evalA y mem
    | NotEqualExpr (x, y) -> evalA x mem <> evalA y mem
    | GreaterExpr (x, y) -> evalA x mem > evalA y mem
    | GreaterEqualExpr (x, y) -> evalA x mem >= evalA y mem
    | LessExpr (x, y) -> evalA x mem < evalA y mem
    | LessEqualExpr (x, y) -> evalA x mem <= evalA y mem

and evalA expr mem =
    match expr with
    | Var (x) -> mem |> Map.find (Var x)
    | Num (x) -> x
    | PlusExpr (x, y) -> evalA x mem + evalA y mem
    | MinusExpr (x, y) -> evalA x mem - evalA y mem
    | TimesExpr (x, y) -> evalA x mem * evalA y mem
    | PowExpr (x, y) -> evalA x mem ** evalA y mem
    | UMinusExpr (x) -> -(evalA x mem)
    | _ -> failwith "Unexpected evalA expr type"

and evalC command mem =
    match command with
    | SkipExpr -> mem
    | AssignExpr (x, expr) ->
                            let memory = mem |> Map.add x (evalA expr mem)
                            memory
    | _ -> failwith "Unexpected evalC command type"

let rec searchEdges (graph: Graph) node =
    match (graph, node) with
    | ([], _) -> []
    | (C (a, b, c) :: gtail, Node (x)) ->
        match a with
        | Node (i) when i = x -> C(a, b, c) :: searchEdges gtail node
        | _ -> searchEdges gtail node
    | (GC (a, b, c) :: gtail, Node (x)) ->
        match a with
        | Node (i) when i = x -> GC(a, b, c) :: searchEdges gtail node
        | _ -> searchEdges gtail node

let rec Inter node listEdges mem edges =
    match listEdges with
    | [] -> mem
    | C (_, b, c) :: _ -> Inter c (searchEdges edges c) (evalC b mem) edges
    | GC (_, b, c) :: _ when (evalB b mem) -> Inter c (searchEdges edges c) mem edges
    | GC (_,_,_) :: tail -> Inter node (tail) mem edges

///////////////////////////////
//PROGRAM VERIFICATION (TASK 4)

// let rec edgesC start finish c coveringNodes=
//     match c with
//     | AssignExpr (x, a) -> ([ C(start, AssignExpr(x, a), finish) ],coveringNodes)
//     | SkipExpr -> ([ C(start, SkipExpr, finish) ],coveringNodes)
//     | SEMIExpr (x, y) ->
//                         let q = fresh ()
//                         let E1,cn = edgesC start q x coveringNodes
//                         let E2,cn2 = edgesC q finish y cn
//                         (E1 @ E2,cn2)
//     | IfExpr (x) -> edgesGC start finish x coveringNodes
//     | DoExpr (x) ->
//                     let b = doneF x
//                     let E,cn = edgesGC start start x (coveringNodes @ [start])
//                     (E @ [ GC(start, b, finish) ],cn)
//     |_ -> [],[]

// and edgesGC start finish gc coveringNodes =
//     match gc with
//     | FunGCExpr (x, y) ->
//                         let q = fresh ()
//                         let E,cn = edgesC q finish y coveringNodes
//                         [ GC(start, x, q) ] @ E,coveringNodes
//     | ElseIfExpr (x, y) ->
//                         let E1,cn = edgesGC start finish x coveringNodes
//                         let E2,cn2 = edgesGC start finish y cn
//                         (E1 @ E2,cn2)

// and doneF a =
//     match a with
//     | FunGCExpr (x, y) -> (NotExpr(x))
//     | ElseIfExpr (x, y) -> AndExpr(doneF x, doneF y)

// let rec ShorestPath (Path : List<Edges>*List<Node>) = 
//     match Path with 
//     | C(x,y,z)::etail,q::ntail -> 

///////////////////////////
//PROGRAM ANALYSIS (TASK 5)

let sign x=
    match x with
    |x_ when x_>0.0 ->"+"
    |x_ when x_=0.0 ->"0"
    |_              ->"-"

let aritOPmult sign1 sign2=
    match (sign1,sign2) with
    |(s1,s2) when (s1="-" && s2="-") || (s1="+" && s2="+") ->Set.empty.Add("+")
    |(s1,s2) when (s1="-" && s2="+") || (s1="+" && s2="-") ->Set.empty.Add("-")
    |(s1,s2) when (s1="0" || s2="0") ->Set.empty.Add("0")
    | _ -> failwith "Unexpected aritOPadd match case"  

let aritOPdiv sign1 sign2=
    match (sign1,sign2) with
    |(s1,s2) when (s1="-" && s2="-") || (s1="+" && s2="+") ->Set.empty.Add("+")
    |(s1,s2) when (s1="-" && s2="+") || (s1="+" && s2="-") ->Set.empty.Add("-")
    |(_,s2)  when s2="0" ->failwith "ERROR: Division by 0!!!"
    |(s1,_)  when s1="0" ->Set.empty.Add("0")
    | _ -> failwith "Unexpected aritOPdiv match case"

let aritOPadd sign1 sign2=
    match (sign1,sign2) with
    |(s1,s2) when (s1="+" && s2="0") || (s1="+" && s2="+") || (s1="0" && s2="+") ->Set.empty.Add("+")
    |(s1,s2) when (s1="-" && s2="0") || (s1="-" && s2="-") || (s1="0" && s2="-") ->Set.empty.Add("-")
    |(s1,s2) when (s1="0" && s2="0") ->Set.empty.Add("0")
    |(s1,s2) when (s1="+" && s2="-") || (s1="-" && s2="+") ->Set.empty.Add("-").Add("+").Add("0")  
    | _ -> failwith "Unexpected aritOPadd match case"  

let aritOPsub sign1 sign2=
    match (sign1,sign2) with
    |(s1,s2) when (s1="+" && s2="0") || (s1="+" && s2="-") || (s1="0" && s2="-") ->Set.empty.Add("+")
    |(s1,s2) when (s1="-" && s2="0") || (s1="0" && s2="+") || (s1="-" && s2="+") ->Set.empty.Add("-")
    |(s1,s2) when (s1="0" && s2="0") ->Set.empty.Add("0")
    |(s1,s2) when (s1="-" && s2="+") || (s1="+" && s2="+") ->Set.empty.Add("-").Add("+").Add("0")  
    | _ -> failwith "Unexpected aritOPsub match case"  

let aritOPpow sign1 sign2=
    match (sign1,sign2) with
    |(_,s2) when s2="0" ->Set.empty.Add("+")
    |(s1,_) when s1="0" ->Set.empty.Add("0")
    |(s1,s2) when s1="-" && s2="+" -> Set.empty.Add("+").Add("-") 
    |(s1,_) when s1="+" -> Set.empty.Add("+").Add("-")  
    | _ -> failwith "Unexpected aritOPpow match case"  

let rec aritOP lst1 lst2 operator=
    match (lst1,lst2,operator) with
    |([],_,_)->Set.empty
    |(_,[],_)->Set.empty
    |(sign1::tail1,sign2::tail2,op) when op="*" -> 
                                                   let set1 = aritOPmult sign1 sign2
                                                   let set2 = Set.union set1 (aritOP [sign1] tail2 "*")
                                                   Set.union set2 (aritOP tail1 ([sign2]@tail2) "*")
    |(sign1::tail1,sign2::tail2,op) when op="/" -> 
                                                   let set1 = aritOPdiv sign1 sign2
                                                   let set2 = Set.union set1 (aritOP [sign1] tail2 "/")
                                                   Set.union set2 (aritOP tail1 ([sign2]@tail2) "/")
    |(sign1::tail1,sign2::tail2,op) when op="+" ->
                                                   let set1 = aritOPadd sign1 sign2
                                                   let set2 = Set.union set1 (aritOP [sign1] tail2 "+")
                                                   Set.union set2 (aritOP tail1 ([sign2]@tail2) "+")
    |(sign1::tail1,sign2::tail2,op) when op="-" ->
                                                   let set1 = aritOPsub sign1 sign2
                                                   let set2 = Set.union set1 (aritOP [sign1] tail2 "-")
                                                   Set.union set2 (aritOP tail1 ([sign2]@tail2) "-")
    |(sign1::tail1,sign2::tail2,op) when op="^" ->
                                                   let set1 = aritOPpow sign1 sign2
                                                   let set2 = Set.union set1 (aritOP [sign1] tail2 "^")
                                                   Set.union set2 (aritOP tail1 ([sign2]@tail2) "^") 
    |_ -> failwith "Unexpected aritOP match case"                                              

let rec signAnalysisA expr absMem =
    // let nonNegArraySigns = Set.empty.Add("+").Add("0")
    match expr with
    |Num(x) -> Set.empty.Add(sign x)
    |Var(x) -> absMem |> Map.find (Var x)
    |TimesExpr(a,b) -> aritOP (Set.toList (signAnalysisA a absMem)) (Set.toList (signAnalysisA b absMem)) ("*")
    |DivExpr(a,b)   -> aritOP (Set.toList (signAnalysisA a absMem)) (Set.toList (signAnalysisA b absMem)) ("/")
    |PlusExpr(a,b)  -> aritOP (Set.toList (signAnalysisA a absMem)) (Set.toList (signAnalysisA b absMem)) ("+")
    |MinusExpr(a,b) -> aritOP (Set.toList (signAnalysisA a absMem)) (Set.toList (signAnalysisA b absMem)) ("-")
    |PowExpr(a,b)   -> aritOP (Set.toList (signAnalysisA a absMem)) (Set.toList (signAnalysisA b absMem)) ("^")
    // |Array(a,b) when not(Set.isEmpty(Set.intersect (signAnalysisA b absMem) nonNegArraySigns)) -> absMem |> Map.find (Var a)
    // |Array(_,b) when Set.isEmpty(Set.intersect (signAnalysisA b absMem) nonNegArraySigns) -> Set.empty
    |_ -> failwith "UnexpectedsignAnalysisA match case"

let boolOPequal sign1 sign2 =
    match (sign1,sign2) with
    |(s1,s2) when s1=s2 -> Set.empty.Add("tt")
    |_ -> Set.empty.Add("ff")

let boolOPgreater sign1 sign2 =
    match (sign1,sign2) with
    |(s1,s2) when (s1="+" && (s2="0" || s2="-")) || (s1="0" && s2="-")-> Set.empty.Add("tt")
    |(s1,s2) when (s1="+" || s1="-") && s2=s1 -> Set.empty.Add("tt").Add("ff")
    |(s1,s2) when (s1="-" || s1="0" ) && (s2="0" || s2="+") -> Set.empty.Add("ff")
    | _ -> failwith "Unexpected boolOPgreater match case"

let boolOPgreaterEq sign1 sign2 =
    match (sign1,sign2) with
    |(s1,s2) when (s1="+" ||s1="0") && (s2="0" || s2="-")-> Set.empty.Add("tt")
    |(s1,s2) when (s1="+" || s1="-") && s2=s1 -> Set.empty.Add("tt").Add("ff")
    |(s1,s2) when (s1="-" && (s2="0" || s2="+")) || (s1="0" && s2="+") -> Set.empty.Add("ff")
    | _ -> failwith "Unexpected boolOPnot match case"

let boolOPand sign1 sign2 =
    match (sign1,sign2) with
    |(s1,s2) when s1="tt" && s2=s1 -> Set.empty.Add("tt")
    |(_,s2) when s2="ff" -> Set.empty.Add("ff")
    | _ -> failwith "Unexpected boolOPand match case" 

let boolOPnot sign1 =
    match sign1 with
    |s1 when s1="tt"->Set.empty.Add("ff")
    |s1 when s1="ff"->Set.empty.Add("tt")
    | _ -> failwith "Unexpected boolOPnot match case"

let rec boolOP lst1 lst2 operator =
    match (lst1,lst2,operator) with
    |(sign1::tail1,sign2::tail2,op) when op="=" -> 
                                                   let set1 = boolOPequal sign1 sign2
                                                   let set2 = Set.union set1 (boolOP [sign1] tail2 "=")
                                                   Set.union set2 (boolOP tail1 ([sign2]@tail2) "=")
    |(sign1::tail1,sign2::tail2,op) when op=">" -> 
                                                   let set1 = boolOPgreater sign1 sign2
                                                   let set2 = Set.union set1 (boolOP [sign1] tail2 ">")
                                                   Set.union set2 (boolOP tail1 ([sign2]@tail2) ">")
    |(sign1::tail1,sign2::tail2,op) when op=">="-> 
                                                   let set1 = boolOPgreaterEq sign1 sign2
                                                   let set2 = Set.union set1 (boolOP [sign1] tail2 ">=")
                                                   Set.union set2 (boolOP tail1 ([sign2]@tail2) ">=")
    |(sign1::tail1,sign2::tail2,op) when op="∧" -> 
                                                   let set1 = boolOPand sign1 sign2
                                                   let set2 = Set.union set1 (boolOP [sign1] tail2 "∧")
                                                   Set.union set2 (boolOP tail1 ([sign2]@tail2) "∧")
    |(sign1::tail1,_,op) when op="not"          -> 
                                                   let set1 = boolOPnot sign1
                                                   Set.union set1 (boolOP tail1 [] "not")
    |([],_,_) ->Set.empty
    |(_,[],_)->Set.empty
    |_ -> failwith "Unexpected boolOP match case"  
    
let rec signAnalysisB bool absMem =
    match bool with
    | True -> Set.empty.Add("tt")
    | EqualExpr(a1,a2) -> boolOP (Set.toList (signAnalysisA a1 absMem)) (Set.toList (signAnalysisA a2 absMem)) "="
    | GreaterExpr(a1,a2) -> boolOP (Set.toList (signAnalysisA a1 absMem)) (Set.toList (signAnalysisA a2 absMem)) ">"
    | GreaterEqualExpr(a1,a2) -> boolOP (Set.toList (signAnalysisA a1 absMem)) (Set.toList (signAnalysisA a2 absMem)) ">="
    | AndExpr(b1,b2) -> boolOP (Set.toList (signAnalysisB b1 absMem)) (Set.toList (signAnalysisB b2 absMem)) "∧"
    // | AndAndExpr of (Boolean * Boolean)
    | AndAndExpr(b1,b2) -> boolOP (Set.toList (signAnalysisB b1 absMem)) (Set.toList (signAnalysisB b2 absMem)) "∧"
    | NotExpr(b1) -> boolOP (Set.toList (signAnalysisB b1 absMem)) [] "not"
    | _ -> failwith "Unexpected signAnalysisB match case"


///////////
//SECURITY ANALYSIS (TASK 6)

let rec fv expr =
    match expr with
    |Num(_) -> Set.empty
    |Var(x)-> Set.empty.Add(x)
    |Array(s,e1) -> fv e1
    |TimesExpr(e1,e2)-> Set.union (fv e1) (fv e2)
    |DivExpr(e1,e2)-> Set.union (fv e1) (fv e2)
    |PlusExpr(e1,e2)-> Set.union (fv e1) (fv e2)
    |MinusExpr(e1,e2)-> Set.union (fv e1) (fv e2)
    |PowExpr(e1,e2)-> Set.union (fv e1) (fv e2)
    |UMinusExpr(e1)-> fv e1
    |UPlusExpr(e1)-> fv e1

let rec fvB bool =
    match bool with
    | True -> Set.empty
    | False -> Set.empty
    | AndExpr(b1,b2) -> Set.union (fvB b1) (fvB b2)
    | AndAndExpr(b1,b2) -> Set.union (fvB b1) (fvB b2)
    | OrExpr(b1,b2) -> Set.union (fvB b1) (fvB b2)
    | OrOrExpr(b1,b2) -> Set.union (fvB b1) (fvB b2)
    | NotExpr(b1) -> fvB b1
    | EqualExpr(e1,e2) -> Set.union (fv e1) (fv e2)
    | NotEqualExpr(e1,e2) -> Set.union (fv e1) (fv e2)
    | GreaterExpr(e1,e2) -> Set.union (fv e1) (fv e2)
    | GreaterEqualExpr(e1,e2) -> Set.union (fv e1) (fv e2)
    | LessExpr(e1,e2) -> Set.union (fv e1) (fv e2)
    | LessEqualExpr(e1,e2) -> Set.union (fv e1) (fv e2)

let rec flowF (lst1: String list) (lst2: String list) =
    match (lst1,lst2) with
    |([],_) -> Set.empty
    |(_,[]) -> Set.empty
    |(h1::t1,h2::t2) -> 
        let set1 = Set.empty.Add((h1,h2))
        let set2 = flowF [h1] t2
        let set3 = flowF t1 ([h2]@t2)
        Set.union (Set.union set1 set2) set3
                                                  
let rec sec command setX =
    match command with
    |AssignExpr(Var(x),e2) -> 
            let f1 = Set.union setX (fv e2)
            let f2 = Set.empty.Add(x)
            flowF (Set.toList f1) (Set.toList f2)
    |ArrayAssignExpr(Array(A,a1),a2) -> 
            let f1 = Set.union setX (fv a1)
            let f2 = Set.union f1 (fv a2)
            let f3 = Set.empty.Add(A)
            flowF (Set.toList f2) (Set.toList f3)
    |SkipExpr-> Set.empty
    |SEMIExpr(c1,c2) -> 
            let set1 = sec c1 setX
            let set2 = sec c2 setX
            Set.union set1 set2
    |IfExpr(gc) ->
            let (w,d) = sec2 gc (False,setX)
            w
    |DoExpr(gc) -> 
            let (w,d) = sec2 gc (False, setX)
            w
    | _ -> failwith "Unexpected sec match case"

and sec2 gcommand (d,setX) =
    match gcommand with
    |FunGCExpr(b,c) -> 
            let w = sec c (Set.union (Set.union setX (fvB b)) (fvB d))
            (w,OrExpr(b,d))
    |ElseIfExpr(gc1,gc2) ->
            let (w1,d1) = sec2 gc1 (d,setX)
            let (w2,d2) = sec2 gc2 (d1,setX)
            (Set.union w1 w2, d2)

let rec allowedFlows (secLattice: Map<String,Set<String>>) secClassification keyList =
    match keyList with
    |[] -> Set.empty
    |key::tail -> let securityLevel = Map.find key secClassification
                  let lattice = Map.find securityLevel secLattice
                  let allowedMap = secClassification |> Map.filter (fun _ security -> lattice.Contains(security) )
                  let set1 = flowF [key] ((allowedMap |> Map.keys) |> Seq.toList)
                  Set.union  set1 (allowedFlows secLattice secClassification tail)

//returns "obvious" security lattice, e.g. private<public to private -> (private,public)
let rec latticeIn (stringList: list<string>) (memo) : Map<String,Set<String>> =
    match stringList with
    | [] -> memo
    | s :: tail ->
        let key = s.Split("<")
        let memory =
            let memory1 = 
                if  (memo |> Map.containsKey key.[0]) 
                then memo |> Map.add (key.[0]) (Set.union (memo |> Map.find key.[0]) (Set.empty.Add(key.[1])))
                else memo |> Map.add (key.[0]) (Set.empty.Add(key.[1]).Add(key.[0]))
            let memory2 =
                if not(memory1 |> Map.containsKey key.[1])
                then memory1 |> Map.add (key.[1]) (Set.empty.Add(key.[1]))
                else memory1
            memory2
        latticeIn tail memory          

//updates the security lattice, e.g. private<public, public<super to private -> (private, public, super)
let rec latticeUpdate (stringList: string list) lattice =
    match stringList with
    |[] -> lattice
    |s::tail ->
        let key = s.Split("<")
        let set1 = lattice |> Map.find key.[0]
        let set2 = lattice |> Map.find key.[1]
        let newLattice = lattice |> Map.add (key.[0]) (Set.union set1 set2)
        latticeUpdate tail newLattice


///////////
//COMPUTING

let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = FM4FUNParser.start FM4FUNLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

let task2Printer e =
    printfn "Enter 1 for the non-deterministic program graph or 2 for the deterministic one:"
    let b = Console.ReadLine()

    if b <> "1" && b <> "2" then
        printfn "Wrong input!"
        failwith "Please try again"
    elif string b = "1" then
        let PG =(convToGraph (edgesC (Node(0)) (Node(-1)) e)).Replace("-1", "End").Replace("Var \"", "Var '").Replace("\",", "',")+ "\n}"
        printfn "\n COPY TO https://dreampuf.github.io/GraphvizOnline/: \n\n digraph G {\n\n%s" PG

    elif string b = "2" then
        let PG =(convToGraph (DEdgesC (Node(0)) (Node(-1)) e)).Replace("\n", " ").Replace("-1", "End").Replace("Var \"", "Var '").Replace("\",", "',").Replace("]", "]\n").Replace("  ", "")+ "\n}"
        printfn "\n COPY TO https://dreampuf.github.io/GraphvizOnline/: \n\n digraph G {\n\n%s" PG

let rec spaceRemover (stringList: list<string>) =
    match stringList with
    | [] -> []
    | s :: tail -> s.Replace(" ", "") :: spaceRemover tail

let rec eqRemover (stringList: list<string>) memo =
    match stringList with
    | [] -> memo
    | s :: tail ->
        let key = s.Split("=")
        let memory = memo |> Map.add (Var(key.[0])) (float (key.[1]))
        eqRemover tail memory

let task3Printer e =
    printfn "Please enter the memory input in the following format:"
    printfn "x = 1, y = 42, z = 3.5 etc..."
    let inputMem = Console.ReadLine()
    let memory_ = Map.empty
    let arr_ = spaceRemover ((inputMem.Split(",")) |> Array.toList)
    let memory = eqRemover arr_ memory_
    let edges = edgesC (Node(0)) (Node(-1)) e
    Inter (Node(0)) (searchEdges edges (Node(0))) (memory) edges

//returns a security classification for variables and arrays.
let rec eqRemover6 (stringList: list<string>) memo =
    match stringList with
    | [] -> memo
    | s :: tail ->
        let key = s.Split("=")
        let memory = memo |> Map.add (key.[0]) (key.[1])
        eqRemover6 tail memory

let task6Printer e =
    printfn "Please enter the security lattice in the following format:"
    printfn "public < private, private < confidential etc..."
    let inputLattice = Console.ReadLine()
    let arr_ = spaceRemover ((inputLattice.Split(",")) |> Array.toList)
    let lattice_ = latticeIn arr_ Map.empty
    let lattice = latticeUpdate arr_ lattice_
    printfn "Please enter the security classificaiton for variables and arrays in the following format:"
    printfn "m = public, n = private, r = confidential etc..."
    let inputClassification = Console.ReadLine()
    let arr2_ = spaceRemover ((inputClassification.Split(",")) |> Array.toList)
    let securityClassification = eqRemover6 arr2_ Map.empty
    printfn "\n\nHERE ARE YOUR RESULTS\n"
    let actualflows =  sec e Set.empty
    printfn "Actual flows: %A" actualflows
    let keyList = (securityClassification |> Map.keys)|> Seq.toList
    let allowedflows = allowedFlows (lattice) (securityClassification) keyList
    printfn "Allowed flows: %A" allowedflows
    if (Set.isSubset actualflows allowedflows) then printfn "The program is secure."
    else printfn "The program is not secure. Violations are: %A" (Set.difference actualflows allowedflows)


let outputFunction i e =
    match i with
    | x when x = "1" -> printfn "Parser for your GCL: %s\n" (printC (e))
    | x when x = "2" -> task2Printer e
    | x when x = "3" -> printfn "Your final memory is: %A" (task3Printer e)
    //| x when x = "4" -> "Sorry, this task has not been implemented."
    //| x when x = "5" -> "Sorry, this task has not been implemented."
    | x when x = "6" -> task6Printer e
    | _ -> printfn "Task not available. Please try again"

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        try

            printfn "Please choose a task to compute: "
            printfn "Enter 1 for task 1 (parser), 2 for task 2 (PG), etc..."
            let task = Console.ReadLine()

            printfn "Please enter an arithmetic expression:"
            let e = parse (Console.ReadLine())

            outputFunction task e
            nodeID <- 1
            compute n
        with
        | err -> compute (n - 1)

// Start interacting with the user
compute 3

//TESTS:
//x:=2+2; if x>2 -> x:=1 [] x=2 -> x:=0 [] x<2 -> x:=-1 fi
//x:=4; y:=1; do x>0 -> y:=x*y; x:=x-1 od
//x:=10; y:=-3; z:=30