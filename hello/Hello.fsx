#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System

#load "HelloParser.fs"

open HelloParser

#load "HelloLexer.fs"

open HelloLexer

let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = HelloParser.start HelloLexer.tokenize lexbuf
    res

let rec who n =
    if n = 0 then
        printfn "Bye bye"
    else
        printfn "Who are you?"
        try
        let name = parse (Console.ReadLine())
        printfn "Hello %s!" name
        with e -> who (n-1)

who 3
