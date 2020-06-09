// Learn more about F# at http://fsharp.org

open Lexer
open Ast


type EnvKey = 
| EnvSymbol of Symbol
| EnvIdentifier of Identifier



let rec evalAst (ast: ScForm list) (env: Map<EnvKey, (double list -> double)>) =
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
            let funcParams = rest |> List.map (evalParam env)  
            
            func funcParams
        | _ -> 
            0.0
and evalParam env form =
    match form with 
    | ScNumber value ->
        value
    | ScList forms ->
        evalAst forms env
    | _ -> 0.0 //Error

// Todo: make type for lambda otherwise can only deal with ints
let env = 
   Map.empty.
      Add(EnvSymbol(MulSym), (fun (argList: double list) -> 
                                argList.Tail |> List.fold ( * ) argList.Head)).
      Add(EnvSymbol(DivSym), (fun (argList: double list) -> 
                                argList.Tail |> List.fold ( / ) argList.Head)).
      Add(EnvSymbol(PlusSym), (fun (argList: double list) -> 
                                argList |> List.sum)).
      Add(EnvSymbol(MinusSym), (fun (argList: double list) -> 
                                argList.Tail |> List.fold ( - ) argList.Head))


[<EntryPoint>]
let main argv =
    let testString = "void Void Test asdfasldfj 1 + 10.1 - 64"
    let lispProg1 = "(print (+ 5 3) (\"test\"))"
    let lispProg2 = "(+ 1 (* 4 5))"
    let lispProg3 = "(* 2 (- 4 (+ 5 6 1)))"
    let tokenList = lexString lispProg3
    let ast = parseTokenList tokenList
    let eval = evalAst ast env
    printfn "%s" lispProg3
    printfn "%A" tokenList
    printfn "%A" ast
    printfn "%A" eval
    0 // return an integer exit code