#r "/Users/maqixin/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System

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
                    let E = edgesGC start start x 
                    E @ [GC(start,b,finish)]
    |_ -> []
    
and edgesGC start finish gc =
    match gc with
    | FunGCExpr(x,y) -> 
                       let q = fresh()
                       let E = edgesC q finish y 
                       [GC (start,x,q)] @ E
    | ElseIfExpr(x,y)->
                       let E1 = edgesGC start finish x
                       let E2 = edgesGC start finish y
                       E1 @ E2
and doneF a = 
    match a with 
    | FunGCExpr (x,y) -> (NotExpr(x))
    | ElseIfExpr (x,y) -> AndExpr (doneF x, doneF y)

let rec convToGraph (lst: Graph)=
    match lst with
    |[] -> []
    |C(a,b,c)::rest -> match (a,c) with
                       |(Node(x),Node(y)) ->"q"+string x+ " -> q"+string y+ " [label = " + string b + "]"::convToGraph rest
    |GC(a,b,c)::rest -> match (a,c) with
                        |(Node(x),Node(y)) ->"q"+string x+ " -> q"+string y+ " [label = " + string b + "]"::convToGraph rest


let rec DEdgesC start finish c =
    match c with
    |AssignExpr(x,a) -> [C(start,AssignExpr(x,a),finish)]
    |SkipExpr -> [C(start,SkipExpr,finish)]
    |SEMIExpr(x,y) ->
                      let q = fresh ()
                      let E1 =DEdgesC start q x
                      let E2 =DEdgesC q finish y
                      E1 @ E2 
                      
    |IfExpr(x) -> 
                  let (E,d) = DEdgesGC start finish x False
                  E
    |DoExpr(x) ->  let (E,d) =DEdgesGC start start x False
                   E @ [GC(start,NotExpr(d),finish)]
    |_ -> []
    
and DEdgesGC start finish gc d=
    match gc with
    | FunGCExpr(x,y) -> 
                       let q = fresh()
                       let E =edgesC q finish y 
                       ([GC (start,AndExpr(x,NotExpr(d)),q)] @ E) , OrExpr(x,d)
    | ElseIfExpr(x,y)->
                       let (E1,d1) =DEdgesGC start finish x d
                       let (E2,d2) =DEdgesGC start finish y d1
                       (E1 @ E2,d2)




