// This script implements our interactive calculator
// mono /Users/maqixin/FsLexYacc.10.0.0/build/fslex/net46/fslex.exe CalculatorLexer.fsl --unicode
// mono /Users/maqixin/FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe CalculatorParser.fsp --module CalculatorParser
// fsharpi Calculator.fsx

// We need to import a couple of modules, including the generated lexer and parser
#r "/Users/maqixin/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
 
open FSharp.Text.Lexing
open System

#load "CalculatorTypesAST.fs"
open CalculatorTypesAST
#load "CalculatorParser.fs"
open CalculatorParser
#load "CalculatorLexer.fs"
open CalculatorLexer

// We define the printAuation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
let rec printA e =
    match e with
    | Num (x) -> string x
    | TimesExpr (x, y) -> "TIMES("+printA(x)+","+printA(y)+")"
    | DivExpr (x, y) -> "DIV("+printA(x)+","+printA(y)+")"
    | PlusExpr (x, y) -> "PLUS("+printA(x)+","+printA(y)+")"
    | MinusExpr (x, y) -> "MINUS("+printA(x)+","+printA(y)+")"
    | PowExpr (x, y) -> "POWER("+printA(x)+","+printA(y)+")"
    | UPlusExpr (x) -> "UPLUS("+printA(x)+")"
    | UMinusExpr (x) -> "UMINUS("+printA(x)+")"
    | Var (x) -> string x                      
    | Array(x,y) -> "ARRAY("+string x+","+printA(y)+")"

let rec printB bool = 
    match bool with 
        | True -> "true"
        | False -> "false"
        // check Formal method syntax
        | AndExpr(x, y) ->  "AND("+printB(x)+","+printB(y)+")"  
        | AndAndExpr(x,y) -> "ANDAND("+printB(x)+","+printB(y)+")"
        | OrExpr(x,y) -> "OR("+printB(x)+","+printB(y)+")"
        | OrOrExpr(x,y) -> "OROR("+printB(x)+","+printB(y)+")" 
        | NotExpr(x) -> "NOT("+printB(x)+")"
        | EqualExpr (x,y) ->"EQUAL("+printA(x)+","+printA(y)+")"
        | NotEqualExpr (x,y) -> "NOTEQUAL("+printA(x)+","+printA(y)+")"
        | GreaterExpr (x,y) ->  "GT("+printA(x)+","+printA(y)+")"
        | GreaterEqualExpr(x,y) ->"GTE("+printA(x)+","+printA(y)+")"
        | LessExpr(x,y)  ->  "LT("+printA(x)+","+printA(y)+")"
        | LessEqualExpr(x,y) -> "LTE("+printA(x)+","+printA(y)+")"



let rec printC (command:Command) =
    match command with 
        | AssignExpr(x,y) ->"ASSIGNVAR("+printA(x)+","+printA(y)+")"
        | ArrayAssignExpr(x,y) ->"ASSIGNARRAY("+printA(x)+","+printA(y)+")"
        | SkipExpr -> "SKIP"
        | SEMIExpr(x,y) -> "SEMICOLON("+printC(x)+","+printC(y)+")"
        | IfExpr(x) -> "IF("+printGC(x)+")"
        | DoExpr(x) -> "DO("+printGC(x)+")"
and  printGC (guardedCommand:GC) =
    match guardedCommand with
        | FunGCExpr(x,y) -> "FUNGC("+printB(x)+","+printC(y)+")"
        | ElseIfExpr(x,y) -> "FUNGC("+printGC(x)+","+printGC(y)+")"


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

let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res =
        CalculatorParser.start CalculatorLexer.tokenize lexbuf
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
            compute n
        with
        | err -> compute (n - 1)

// Start interacting with the user
compute 3
