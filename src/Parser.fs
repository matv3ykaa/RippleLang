module RippleLang.Parser

open RippleLang.AST
open RippleLang.Lexer

type Parser(tokens: Token list) =
    let mutable current = 0
    let mutable tokenList = tokens

    member private this.IsAtEnd() =
        current >= List.length tokenList || tokenList[current] = EOF

    member private this.Peek() =
        if this.IsAtEnd() then EOF else tokenList[current]

    member private this.Advance() =
        if not (this.IsAtEnd()) then current <- current + 1
        this.Previous()

    member private this.Previous() =
        if current = 0 then EOF else tokenList[current - 1]

    member private this.Match(types: Token list) =
        if types |> List.exists (fun t -> this.Peek() = t) then
            this.Advance() |> ignore
            true
        else false

    member private this.Consume(tokenType, message) =
        if this.Peek() = tokenType then this.Advance()
        else failwith message

    member this.ParseProgram() =
        let rec parseExprs acc =
            if this.IsAtEnd() then List.rev acc
            else
                let expr = this.ParseExpression()
                if this.Match([SEMICOLON]) then parseExprs (expr :: acc)
                else parseExprs (expr :: acc)
        parseExprs []

    member private this.ParseExpression() =
        this.ParseLet()

    member private this.ParseLet() =
        if this.Match([LET]) then
            let isRec = this.Match([REC])
            let name =
                match this.Advance() with
                | IDENT s -> s
                | _ -> failwith "Expected identifier"
            this.Consume(ASSIGN, "Expected '='") |> ignore
            let value = this.ParseExpression()
            this.Consume(IN, "Expected 'in'")  |> ignore
            let body = this.ParseExpression()
            if isRec then LetRec(name, value, body)
            else Let(name, value, body)
        else this.ParseIf()

    member private this.ParseIf() =
        if this.Match([IF]) then
            let cond = this.ParseEquality()
            this.Consume(THEN, "Expected 'then'") |> ignore
            let thenBr = this.ParseExpression()
            this.Consume(ELSE, "Expected 'else'") |> ignore
            let elseBr = this.ParseExpression()
            If(cond, thenBr, elseBr)
        else this.ParseLambda()

    member private this.ParseLambda() =
        if this.Match([LAMBDA]) then
            let param =
                match this.Advance() with
                | IDENT s -> s
                | _ -> failwith "Expected parameter"
            this.Consume(ARROW, "Expected '->'")  |> ignore
            let body = this.ParseExpression()
            Lambda(param, body)
        else this.ParseLogical()
        
    // Added method for logical operators (AND, OR)
    member private this.ParseLogical() =
        let mutable expr = this.ParseComparison()
        while this.Match([AND; OR]) do
            let op = 
                match this.Previous() with
                | AND -> And
                | OR -> Or
                | _ -> failwith "Unexpected operator"
            expr <- Op(expr, op, this.ParseComparison())
        expr
        
    // Added method for comparison operators
    member private this.ParseComparison() =
        let mutable expr = this.ParseEquality()
        while this.Match([GREATER; GREATEREQUAL; LESS; LESSEQUAL]) do
            let op =
                match this.Previous() with
                | GREATER -> Gt
                | GREATEREQUAL -> Gte
                | LESS -> Lt
                | LESSEQUAL -> Lte
                | _ -> failwith "Unexpected operator"
            expr <- Op(expr, op, this.ParseEquality())
        expr

    member private this.ParseEquality() =
        let mutable expr = this.ParseAdditive()
        while this.Match([EQUAL; NOTEQUAL]) do
            let op =
                match this.Previous() with
                | EQUAL -> Eq
                | NOTEQUAL -> Neq
                | _ -> failwith "Unexpected operator"
            expr <- Op(expr, op, this.ParseAdditive())
        expr

    member private this.ParseAdditive() =
        let mutable expr = this.ParseMultiplicative()
        while this.Match([PLUS; MINUS]) do
            let op =
                match this.Previous() with
                | PLUS -> Add
                | MINUS -> Sub
                | _ -> failwith "Unexpected operator"
            expr <- Op(expr, op, this.ParseMultiplicative())
        expr

    member private this.ParseMultiplicative() =
        let mutable expr = this.ParseApp()
        while this.Match([STAR; SLASH; PERCENT]) do
            let op =
                match this.Previous() with
                | STAR -> Mul
                | SLASH -> Div
                | PERCENT -> Mod
                | _ -> failwith "Unexpected operator"
            expr <- Op(expr, op, this.ParseMultiplicative())
        expr

    // Fixed method for function application
    member private this.ParseApp() =
        let mutable expr = this.ParsePrimary()
        
        // Continue parsing as long as we have valid application expressions following
        while not (this.IsAtEnd()) && 
              (match this.Peek() with 
               | LPAREN | INT _ | FLOAT _ | STRING _ | TRUE | FALSE 
               | IDENT _ | LBRACKET -> true 
               | _ -> false) do
            
            // Parse argument
            let arg = this.ParsePrimary()
            
            // Apply function to argument
            expr <- Apply(expr, arg)
            
        expr

    member private this.ParsePrimary() =
        match this.Advance() with
        | INT i -> Literal(LInt i)
        | FLOAT f -> Literal(LFloat f)
        | STRING s -> Literal(LString s)
        | TRUE -> Literal(LBool true)
        | FALSE -> Literal(LBool false)
        | IDENT name -> Variable name
        | LPAREN ->
            if this.Match([RPAREN]) then
                Literal(LUnit)  // Empty parentheses represent unit value
            else
                let expr = this.ParseExpression()
                this.Consume(RPAREN, "Expected ')'")  |> ignore
                expr
        | LBRACKET ->
            let mutable items = []
            while not (this.Match([RBRACKET])) do
                items <- items @ [this.ParseExpression()]
                if not (this.Match([COMMA])) && this.Peek() <> RBRACKET then
                    failwith "Expected ',' or ']'"
            List items
        | token -> failwithf "Unexpected token: %A" token

let parse source =
    let tokens = tokenize source
    let parser = Parser(tokens)
    parser.ParseProgram()