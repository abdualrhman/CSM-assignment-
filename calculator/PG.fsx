
#r "/Users/maqixin/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
//git status 
//git add .
//git commit -m "task2 PG"
//git push 
#load "FM4FUNTypesAST.fs"
open FM4FUNTypesAST

#load "FM4FUNParser.fs"
open FM4FUNParser
#load "FM4FUNLexer.fs"
open FM4FUNLexer


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
//let mutable EList = []

let rec edgesC start finish c =
        match c  with
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
    
and edgesGC start finish gc =
    match gc  with
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
    |C(a,b,c)::rest  -> string a+" -> "+string c + "[ label ="+ string b + "]"::convToGraph rest
    |GC(a,b,c)::rest -> string a+" -> "+string c + "[ label ="+ string b + "]"::convToGraph rest

