
#r "FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"

open Microsoft.FSharp.Text.Lexing
open System

#load "CalculatorTypesAST.fs"
open CalculatorTypesAST

#load "CalculatorParser.fs"

open CalculatorParser

#load "CalculatorLexer.fs"

open CalculatorLexer

let x = " 1.0 + 1.0"

let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = CalculatorParser.start CalculatorLexer.tokenize lexbuf
    res

let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printfn "Enter an arithmetic expression: "
        try
        let r = parse (Console.ReadLine())
        printfn "Result: %A" r
        with e -> compute (n-1)

compute 3
