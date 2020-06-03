module Lexer

open System
open System.Collections
open System.Text.RegularExpressions

type Token =
    | WhiteSpace
    | EOF
    | SOF //Start Of File
    | Symbol of Symbol
    | Number of Number
    | Identifier of Identifier
    | Invalid of string
and Symbol =
    | MulSym
    | DivSym
    | PlusSym
    | MinusSym
    | EqualSym
    | OpenParenSym
    | CloseParenSym
    override x.ToString() =
        match x with
        | MulSym  -> "*"
        | DivSym  -> "/"
        | PlusSym   -> "+"
        | MinusSym -> "-"
        | EqualSym -> "="
        | OpenParenSym -> "("
        | CloseParenSym -> ")"
and Number =
    | IntNum of int32
    | DoubleNum of double
    | IncompleteNum
and Identifier =
    | TypeSpec of TypeSpec
    | UserDefId of string
    | IncompleteId
and TypeSpec =
    | DoubleType
    | VoidType
    | IntType
    override x.ToString() =
        match x with
        | DoubleType  -> "double"
        | VoidType -> "void"
        | IntType -> "int"


let private constructStringTok tok tokStr = 
    match tok with
    | Number ->
        let (|DoubleStr|IntStr|None|) =
            let doubleReg = Regex "[0-9]+\.[0-9]+"
            let intReg = Regex "[0-9]+"
            fun input ->
                if doubleReg.IsMatch input then DoubleStr
                elif intReg.IsMatch input then IntStr
                else None

        match tokStr with
        | DoubleStr ->
            Number(DoubleNum(Double.Parse tokStr))
        | IntStr ->
            Number(IntNum(Int32.Parse tokStr))
        | None ->
            Invalid(tokStr)
    | Identifier ->
        match tokStr with
        | "void" -> Identifier(TypeSpec(VoidType))
        | "int"  -> Identifier(TypeSpec(IntType))
        | "double" -> Identifier(TypeSpec(DoubleType))
        | _ -> Identifier(UserDefId(tokStr))
    | _ -> 
        Invalid(tokStr)

// Uses mutable Queue
// Not thread safe
let rec private lex (str: char list) (tokenQueue: Queue) (curTokType: Token) (curTokStr: string) =

    let (|DigitCh|LetterCh|WhiteSpaceCh|OtherCh|) ch = 
       if Char.IsDigit(ch) then DigitCh
       else if Char.IsLetter(ch) then LetterCh
       else if Char.IsWhiteSpace(ch) then WhiteSpaceCh
       else OtherCh

    match str with
    | [] ->
        // Exit cond
        tokLogic [] tokenQueue EOF ' '
            curTokType curTokStr
    | nextChar::rest ->
        let nextTokType = 
            match nextChar with
            | '+' ->
                Symbol(PlusSym)
            | '-' ->
                Symbol(MinusSym)
            | '*' ->
                Symbol(MulSym)
            | '/' ->
                Symbol(DivSym)
            | '=' ->
                Symbol(EqualSym)
            | '(' ->
                Symbol(OpenParenSym)
            | ')' ->
                Symbol(CloseParenSym)
            | WhiteSpaceCh-> 
                WhiteSpace
            | DigitCh |'.' ->
                Number(IncompleteNum)
            | LetterCh ->
                Identifier(IncompleteId)
            | _ ->
                Invalid(string char)

        tokLogic rest tokenQueue nextTokType nextChar
            curTokType curTokStr         
and tokLogic (rest: char list) (tokenQueue: Queue) (nextTokType: Token) (nextChar: char)
    (curTokType: Token) (curTokStr: string) =

    let nextCharStr = string nextChar

    match curTokType with
        | Number | Identifier ->
            if curTokType = nextTokType then 
                lex rest 
                    tokenQueue nextTokType 
                    (curTokStr + string nextChar)
            else
                // Construct and append current string token
                tokenQueue.Enqueue (constructStringTok curTokType curTokStr)
                lex rest
                    tokenQueue nextTokType (string nextChar)
        | Symbol | Invalid | SOF ->
            tokenQueue.Enqueue curTokType
            lex rest
                tokenQueue nextTokType nextCharStr
        | WhiteSpace ->
            if nextTokType <> WhiteSpace then
                tokenQueue.Enqueue curTokType
            lex rest
                tokenQueue nextTokType nextCharStr
        | EOF ->
            // Exit cond
            tokenQueue.Enqueue EOF
            tokenQueue.ToArray()


    
let lexString str = 
    lex (Seq.toList str) (Queue()) SOF ""

