type Program = Form of Form list

and Form = 
    | Definition of Definition
    | Expression of Expression

and Definition = 
    | VariableDef of VariableDef
    // (define-syntax <keyword> <transformer expression>)
    | SyntaxDef of Identifier * TransformerExpr
    // (begin <Definition>*)
    | BeginDef of Definition list
    // (let-syntax (<syntax binding>*) <definition>*)
    | LetSyntaxDef of SyntaxBinding list * Definition list
    // (letrec-syntax (<syntax binding>*) <definition>*)
    | LetSyntaxRecDef of SyntaxBinding list * Definition list
    // (<keyword> <transformer expression>)  
    | SyntaxBinding of Identifier * TransformerExper
    | DerivedDef
and VariableDef = 
    // (define <variable> <expression>)
    | DefineVar of Variable * Expression
    // (define (<variable> <variable>*) <body>)
    | DefineFunc of Variable * Variable list * Body
    // (define (<variable> <variable>* . <variable>) <body>)
and Expression =
    | Constant
    | Variable
    // (Quote <datum>)| '<datum>
    | Quote of Datum
    | Lambda of Formals * Body