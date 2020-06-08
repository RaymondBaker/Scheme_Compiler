module Ast

open Lexer


type ScForm =
| ScList of ScForm list 
| ScSymbol of Symbol
| ScNumber of double
| ScParseError of string


let private readAtom (tokenList: Token list) = 
    match tokenList with
    | [] -> ([], [ScParseError("Atom was passed empty tokenList")])
    | token::rest -> 
        match token with 
        | Symbol sym ->
            (rest, [ScSymbol(sym)])
        | Number num ->
            match num with 
            | DoubleNum value -> 
                (rest, [ScNumber(value)])
            | IntNum value ->
                (rest, [ScNumber(double value)])
            | _ -> ([], [ScParseError("Unknown Number " + token.ToString())])
        // Remove this as all cases should be covered
        | _ -> ([], [ScParseError("Unknown Token " + token.ToString())])

let rec private readForm (tokenList: Token list) (formList: ScForm list) =
    match tokenList with
    // Also error because it should see eof
    | [] -> ([], formList) 
    | token::rest ->
        match token with
        | Symbol OpenParenSym -> 
            let (retRest, retFormList) = readList rest []
            readForm retRest (formList @ retFormList)
        | Symbol CloseParenSym ->
            (tokenList, formList)
        // Exit Conds
        | EOF ->
            (rest, formList)
        | SOF | WhiteSpace ->
            readForm rest formList
        | _ -> 
            let (retRest, retFormList) = readAtom tokenList 
            readForm retRest (formList @ retFormList)


and readList (tokenList: Token list) (formList: ScForm list) = 
    match tokenList with
    // This is an error as it should see a ) before ending
    | [] -> ([], formList) 
    | token::rest ->
        match token with
        | Symbol CloseParenSym -> (rest, formList)
        | _ -> 
            let (retRest, retFormList) = readForm tokenList []
            let newFormList = ScList(retFormList) 
            readList retRest (formList @ [newFormList])


let parseTokenList (tokenList: Token list) = 
    let (_, parsed) = readForm tokenList []
    parsed