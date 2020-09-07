// Learn more about F# at http://fsharp.org

open System
open FParsec

type Ast =
    | Value of int64
    | Product of Ast * Ast list
    | Sum of Ast * Ast list

let expr, exprRef = createParserForwardedToRef()

let value =
    (pint64  |>> Ast.Value ) <|> between (pchar '(') (pchar ')') expr

let product =
    let op = pchar '*' <|> pchar '/'
    (value .>>. (many (op >>. value))) |>> Ast.Product

let sum =
    let op = pchar '+' <|> pchar '-'
    (product .>>. (many (op >>. product))) |>> Ast.Sum

exprRef := sum

[<EntryPoint>]
let main argv =

    run expr "123" |> printfn "%A"
    run expr "1*2" |> printfn "%A"
    run expr "1+3*4" |> printfn "%A"
    run expr "(1+2)*3" |> printfn "%A"

    0 // return an integer exit code
