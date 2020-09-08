// Flop - Sometimes you fail, and that's OK
//
// This is a small investigation into error handling and recovery in FParsec
// based on the ideas discussed by @ebkalderon in [Error recovery with parser
// combinators (using nom)](https://www.eyalkalderon.com/nom-error-recovery/)

open System
open FParsec

/// Sructured error information to be emitted by our parser.
type Diagnostic = Diagnostic of Position * string

/// The output of our parse.
type SyntaxNode =
    | Value of int64
    | Product of SyntaxNode * SyntaxNode list
    | Sum of SyntaxNode * SyntaxNode list
    | Error


/// Our Parser State. Used to keep track of the diagnostics we encountered while
/// parsing the source text so far.
type State =
    { mutable Diagnostics: Diagnostic list }

    /// Emit Diagnostic
    member s.EmitDiagnostic pos err =
        let diag = Diagnostic(pos, err)
        s.Diagnostics <- diag::s.Diagnostics

    /// Initial parser state
    static member Initial = { Diagnostics = [] }

let expect (p: Parser<'a, State>) err =
    let raiseErr (stream: CharStream<State>) =
        stream.UserState.EmitDiagnostic stream.Position err
        Reply(None)
    attempt p |>> Some <|> raiseErr

let expectSyn (p: Parser<SyntaxNode, State>) err =
    expect p err |>> Option.defaultValue Error

let expr, exprRef = createParserForwardedToRef()

let value =
    (pint64  |>> Value ) <|> between (pchar '(') (expect (pchar ')') "missing closing ')'") (expectSyn expr "expected expression after opening (")

let product =
    let op = pchar '*' <|> pchar '/'
    (value .>>. (many (op >>. expectSyn value "expected expression after operator"))) |>> Product

let sum =
    let op = pchar '+' <|> pchar '-'
    (product .>>. (many (op >>. expectSyn product "expected expression after operator"))) |>> Sum

exprRef := sum

let parser = expr .>> eof

/// Runs the parser on the input string and throws an exception if the parser
/// fails. We expect the parser should _always_ succeed. For malformed source
/// text an `SyntaxNode.Error` should be returned and a `Diagnostic` emitted.
let private parseExpr input =
    match runParserOnString parser State.Initial "test" input with
    | ParserResult.Failure(_) as f -> failwithf "Parser failed! %A" f
    | ParserResult.Success(r, s, _) -> (r, s.Diagnostics)

let private test input =
    parseExpr input
    |> printf "parsed %A"

[<EntryPoint>]
let main argv =

    test "123"
    test "1*2"
    test "1+3*4"
    test "(1+2)*3"
    test "1+"
    test "(1"
    test "()"
    test "("

    0 // return an integer exit code
