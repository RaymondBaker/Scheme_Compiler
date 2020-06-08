// Learn more about F# at http://fsharp.org

open Lexer
open Ast


type EnvKey = 
| EnvSymbol of Symbol
| EnvIdentifier of Identifier

let formToDouble form =
    match form with 
    | ScNumber value ->
        value
    | _ ->
        // ERROR
        -69.0

let rec evalAst (ast: ScForm list) (env: Map<EnvKey, (double -> double -> double)>) =
    match ast with 
    | [] -> 0.0
    | form::rest -> 
        printfn "%A" form
        match form with
        | ScList formList ->
            evalAst formList env
        | ScSymbol sym ->
            //check if this finds something
            let func = env.TryFind(EnvSymbol(sym)).Value
            let firstParam = (formToDouble rest.Head)
            let secondParam = (formToDouble rest.Tail.Head)  
            
            func firstParam secondParam
        | _ -> 
            0.0

// Todo: make type for lambda otherwise can only deal with ints
let env = 
   Map.empty.
      Add(EnvSymbol(MulSym), (fun a b -> a * b)).
      Add(EnvSymbol(DivSym), (fun a b -> a / b)).
      Add(EnvSymbol(PlusSym), (fun a b -> a + b)).
      Add(EnvSymbol(MinusSym), (fun a b -> a - b))


[<EntryPoint>]
let main argv =
    let testString = "void Void Test asdfasldfj 1 + 10.1 - 64"
    let lispProg1 = "(print (+ 5 3) (\"test\"))"
    let lispProg2 = "(+ 1 (* 4 5))"
    let lispProg3 = "(+ 29 5)"
    let tokenList = lexString lispProg3
    let ast = parseTokenList tokenList
    let eval = evalAst ast env
    printfn "%s" lispProg3
    printfn "%A" tokenList
    printfn "%A" ast
    printfn "%A" eval
    0 // return an integer exit code