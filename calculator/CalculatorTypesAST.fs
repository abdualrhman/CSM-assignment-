// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module CalculatorTypesAST

type expr =
    | Num of float
    | Var of string
    | Array of (string * expr)
    | TimesExpr of (expr * expr)
    | DivExpr of (expr * expr)
    | PlusExpr of (expr * expr)
    | MinusExpr of (expr * expr)
    | PowExpr of (expr * expr)
    | UPlusExpr of (expr)
    | UMinusExpr of (expr)

type Boolean =
    | True   
    | False  
    | AndExpr            of (Boolean * Boolean)
    | AndAndExpr         of (Boolean * Boolean)
    | OrExpr             of (Boolean * Boolean)  
    | OrOrExpr           of (Boolean * Boolean)
    | NotExpr            of  (Boolean)  
    | EqualExpr          of (expr * expr)
    | NotEqualExpr       of (expr * expr)
    | GreaterExpr        of (expr * expr)
    | GreaterEqualExpr   of (expr * expr)
    | LessExpr           of (expr * expr)
    | LessEqualExpr      of (expr * expr)

type Command = 
    | AssignExpr        of (expr * expr)
    | ArrayAssignExpr   of (expr * expr)
    | SkipExpr          
    | SEMIExpr          of (Command * Command)
    | IfExpr            of (GC)
    | DoExpr            of (GC)
type GC = 
    | FunGCExpr             of (Boolean * Command)
    | ElseIfExpr            of  (GC * GC)



