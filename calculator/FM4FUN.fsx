// This script implements our interactive calculator

// mono /Users/maqixin/FsLexYacc.10.0.0/build/fslex/net46/fslex.exe FM4FUNLexer.fsl --unicode
// mono /Users/maqixin//FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe FM4FUNParser.fsp --module FM4FUNParser
// fsharpi FM4FUN.fsx

// We need to import a couple of modules, including the generated lexer and parser
#r "/Users/maqixin/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System

#load "FM4FUNTypesAST.fs"
open FM4FUNTypesAST
#load "FM4FUNParser.fs"
open FM4FUNParser
#load "FM4FUNLexer.fs"
open FM4FUNLexer
//#load "PG.fsx"
//open PG

// We define the printAuation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
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
    | Var (x) -> string x
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
    | ArrayAssignExpr (x, y) ->
        "ASSIGNARRAY("
        + printA (x)
        + ","
        + printA (y)
        + ")"
    | SkipExpr -> "SKIP"
    | SEMIExpr (x, y) -> "SEMICOLON(" + printC (x) + "," + printC (y) + ")"
    | IfExpr (x) -> "IF(" + printGC (x) + ")"
    | DoExpr (x) -> "DO(" + printGC (x) + ")"

and printGC (guardedCommand: GC) =
    match guardedCommand with
    | FunGCExpr (x, y) -> "FUNGC(" + printB (x) + "," + printC (y) + ")"
    | ElseIfExpr (x, y) -> "FUNGC(" + printGC (x) + "," + printGC (y) + ")"









// (AST of type printB)
(*let rec printB bool =
    match bool with
        | True -> true
        | False -> false
        // check Formal method syntax
        | AndExpr(x, y) ->  if printB(x) then printB(y) else false
        | AndAndExpr(x,y) -> printB(x) && printB(y)
        | OrExpr(x,y) -> if not(printB(x)) then printB(y) else true
        | OrOrExpr(x,y) -> printB(x) || printB(y)
        | NotExpr(x) -> not(printB(x))
        | EqualExpr (x,y) -> printA(x) = printA(y) //"EQUAL ("+printA(x)+","+printA(y)+")ss"
        | NotEqualExpr (x,y) -> printA(x) <> printA(y)
        | GreaterExpr (x,y) ->  printA(x) > printA(y)
        | GreaterEqualExpr(x,y) ->printA(x) >= printA(y)
        | LessExpr(x,y)  ->  printA (x) < printA(y)
        | LessEqualExpr(x,y) -> printA(x) <= printA(y)


*)

type Node  = Node of int


type Edges =
    | C of Node * Command * Node
    | GC of Node * Boolean *Node

type Graph =  Edges List

let mutable nodeID = 1

let fresh () =
        let curNode = nodeID
        nodeID <- curNode + 1
        Node  (curNode)

let rec edgesC start finish c =
    match c with
    |AssignExpr(x,a) -> [C(start,AssignExpr(x,a),finish)]
    |SkipExpr -> [C(start,SkipExpr,finish)]
    |SEMIExpr(x,y) ->
                      let q = fresh ()
                      let E1 = edgesC start q x
                      let E2 = edgesC q finish y
                      E1 @ E2 
                      
    |IfExpr(x) -> edgesGC start finish x
    |DoExpr(x) -> 
                    let b = doneF x
                    let E = edgesGC start finish x 
                    E @ [GC(start,b,finish)]
    |_ -> []
    
and edgesGC start finish gc =
    match gc with
    | FunGCExpr(x,y) -> 
                       let q = fresh()
                       let E = edgesC q finish y 
                       E @ [GC (start,x,q)]
    | ElseIfExpr(x,y)->
                       let E1 = edgesGC start finish y
                       let E2 = edgesGC start finish y
                       E1 @ E2
and doneF a = 
    match a with 
    | FunGCExpr (x,y) -> (NotExpr(x))
    | ElseIfExpr (x,y) -> AndExpr (doneF x, doneF y)

let rec convToGraph lst=
    match lst with
    |[] -> []
    |C(a,b,c)::rest -> string a+" -> "+string c + "[ label ="+ string b + "]"::convToGraph rest
    |GC(a,b,c)::rest -> string a+" -> "+string c + "[ label ="+ string b + "]"::convToGraph rest


let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res =
        FM4FUNParser.start FM4FUNLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter an arithmetic expression: "

        try
            // We parse the input string
            let e = parse (Console.ReadLine())
            // and print the result of printAuating it
            printfn "Result: %s" (printC (e))
            printfn "List = %A" (convToGraph (edgesC (Node(0)) (Node(-1)) e))
            compute n
        with
        | err -> compute (n - 1)

// Start interacting with the user
compute 3

//x:=2+2; if x>2 -> x:=1 [] x=2 -> x:=0 [] x<2 -> x:=-1 fi