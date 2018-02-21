
#r "FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"

open Microsoft.FSharp.Text.Lexing
open System

#load "CalculatorTypesAST.fs"
open CalculatorTypesAST

#load "CalculatorParser.fs"

open CalculatorParser

#load "CalculatorLexer.fs"

open CalculatorLexer

let rec eval e =
  match e with
    | Num(x) -> x
    | TimesExpr(x,y) -> eval(x) * eval (y)
    | DivExpr(x,y) -> eval(x) / eval (y)
    | PlusExpr(x,y) -> eval(x) + eval (y)
    | MinusExpr(x,y) -> eval(x) - eval (y)
    | PowExpr(x,y) -> eval(x) ** eval (y)

let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = CalculatorParser.start CalculatorLexer.tokenize lexbuf
    res

let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter an arithmetic expression: "
        try
        let e = parse (Console.ReadLine())
        printfn "Result: %f" (eval(e))
        compute n
        with err -> compute (n-1)

compute 3
