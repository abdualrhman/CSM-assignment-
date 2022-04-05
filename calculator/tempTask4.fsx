// let rec edgesC start finish c coveringNodes=
//     match c with
//     | AssignExpr (x, a) -> [ C(start, AssignExpr(x, a), finish) ]
//     | SkipExpr -> [ C(start, SkipExpr, finish) ]
//     | SEMIExpr (x, y) ->
//                         let q = fresh ()
//                         let E1,cn = edgesC start q x coveringNodes
//                         let E2,cn2 = edgesC q finish y cn
//                         (E1 @ E2,cn2)
//     | IfExpr (x) -> edgesGC start finish x
//     | DoExpr (x) ->
//                     let b = doneF x
//                     let E,cn = edgesGC start start x (coveringNodes::start)
//                     (E @ [ GC(start, b, finish) ],cn)
//     | _ -> []

// and edgesGC start finish gc =
//     match gc with
//     | FunGCExpr (x, y) ->
//                         let q = fresh ()
//                         let E = edgesC q finish y
//                         [ GC(start, x, q) ] @ E
//     | ElseIfExpr (x, y) ->
//                         let E1 = edgesGC start finish x
//                         let E2 = edgesGC start finish y
//                         E1 @ E2

// and doneF a =
//     match a with
//     | FunGCExpr (x, y) -> (NotExpr(x))
//     | ElseIfExpr (x, y) -> AndExpr(doneF x, doneF y)
