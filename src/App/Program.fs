// Learn more about F# at http://fsharp.org

open System

type Token =
    |   Whitespace
    |   Operator of string
    |   Number of double
    |   Identifier of string
    |   Invalid of char

let constructTok tokType tokStr =
    match tokType with
    | Whitespace -> Whitespace
    | Number     -> Number(double tokStr)
    | Invalid(c) -> Invalid(c)
    | Identifier -> Identifier(tokStr)
    | Operator   -> Operator(tokStr) 

let rec lex (str : char list) (tokenList : Token list) (lastTokType : Token) (curTokStr : string) =
    // Exit cond
    match str with
    // This is VERY INEFFICIENT look for other data type than list
    | [] -> tokenList @ [(constructTok lastTokType curTokStr)]
    | char::rest ->
        let curTokType = 
            match char with
            | x when Char.IsWhiteSpace(x) -> 
                Whitespace
            | x when Char.IsDigit(x) || x = '.' ->
                Number(0.0)
            | '+' | '-' | '*' | '/' | '=' ->
                Operator("Incomplete")
            | _ ->
                Invalid(char) 

        // Use match statement to see if this is a multi char token
        if curTokType = lastTokType || curTokStr = "" then 
            printfn "%A %s" curTokType curTokStr 
            lex rest 
                tokenList curTokType 
                (curTokStr + string char)
        else
            printfn "%A %s" curTokType curTokStr 
            // Construct and append current token
            lex rest
                // This is VERY INEFFICIENT look for other data type than list
                (tokenList @ [(constructTok lastTokType curTokStr)])
                curTokType (string char)
    
    
let lexString str = 
    lex (Seq.toList str) [] (Invalid('e')) ""



[<EntryPoint>]
let main argv =
    let testString = "1 + + ++ 1"
    let expr = lexString testString
    printfn "%A" expr
    0 // return an integer exit code