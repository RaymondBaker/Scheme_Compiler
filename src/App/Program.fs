// Learn more about F# at http://fsharp.org

open Lexer




[<EntryPoint>]
let main argv =
    let testString = "void Void Test asdfasldfj 1 + 10.1 - 64"
    let expr = Lexer.lexString testString
    printfn "%s" testString
    printfn "%A" expr
    0 // return an integer exit code