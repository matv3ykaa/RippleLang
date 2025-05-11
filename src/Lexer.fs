module DropletLang.Lexer

open System
open System.Text.RegularExpressions

type Token =
    | IDENT of string
    | INT of int
    | FLOAT of float
    | STRING of string
    | FLOW       // Вместо LAMBDA
    | ARROW
    | LET
    | REC
    | IN
    | WHEN       // Вместо IF
    | THEN
    | ELSE
    | DRIP       // Новый токен для циклов
    | TRUE
    | FALSE
    | LPAREN
    | RPAREN
    | LBRACKET
    | RBRACKET
    | COMMA
    | SEMICOLON
    | ASSIGN
    | PLUS
    | MINUS
    | STAR
    | SLASH
    | PERCENT
    | EQUAL
    | NOTEQUAL
    | GREATER
    | LESS
    | GREATEREQUAL
    | LESSEQUAL
    | AND
    | OR
    | CONS
    | ABSORB    // Для импорта
    | EOF

let tokenize (input: string) : Token list =
    let patterns = [
        @"(\s|#.*)+", fun _ -> None  // Обрабатывает пробелы и комментарии (# вместо //)
        @"\bwhen\b", fun _ -> Some WHEN
        @"\bthen\b", fun _ -> Some THEN
        @"\belse\b", fun _ -> Some ELSE
        @"\blet\b", fun _ -> Some LET
        @"\brec\b", fun _ -> Some REC
        @"\bin\b", fun _ -> Some IN
        @"\bflow\b", fun _ -> Some FLOW
        @"\bdrip\b", fun _ -> Some DRIP
        @"\babsorb\b", fun _ -> Some ABSORB
        @"\btrue\b", fun _ -> Some TRUE
        @"\bfalse\b", fun _ -> Some FALSE
        @"\(", fun _ -> Some LPAREN
        @"\)", fun _ -> Some RPAREN
        @"\[", fun _ -> Some LBRACKET
        @"\]", fun _ -> Some RBRACKET
        @",", fun _ -> Some COMMA
        @";", fun _ -> Some SEMICOLON
        @"->", fun _ -> Some ARROW
        @"==", fun _ -> Some EQUAL
        @"!=", fun _ -> Some NOTEQUAL
        @">=", fun _ -> Some GREATEREQUAL
        @">", fun _ -> Some GREATER
        @"<=", fun _ -> Some LESSEQUAL
        @"<", fun _ -> Some LESS
        @"&&", fun _ -> Some AND
        @"\|\|", fun _ -> Some OR
        @"::", fun _ -> Some CONS
        @"=", fun _ -> Some ASSIGN
        @"\+", fun _ -> Some PLUS
        @"-", fun _ -> Some MINUS
        @"\*", fun _ -> Some STAR
        @"/", fun _ -> Some SLASH
        @"%", fun _ -> Some PERCENT
        @"[0-9]+\.[0-9]+", fun s -> Some(FLOAT(float s))
        @"[0-9]+", fun s -> Some(INT(int s))
        @"""([^""]*)""", fun (s: string) ->
            if s.Length >= 2 then
                Some(STRING(s.Substring(1, s.Length - 2).Replace("\"\"", "\"")))
            else
                failwith "Invalid string literal"
        @"[a-zA-Z_][a-zA-Z0-9_]*", fun s -> Some(IDENT s)
        @"[\p{L}_][\p{L}0-9_]*", fun s -> Some(IDENT s)
    ]

    let rec tokenizeRest position tokens (inputStr: string) =
        if position >= inputStr.Length then
            let result = List.rev (EOF :: tokens)
            printfn "Tokens: %A" result  // Отладочный вывод
            result
        else
            let inputSubstring = inputStr.Substring(position)
            let mutable matched = false
            let mutable newPosition = position
            let mutable newTokens = tokens

            for (pattern, action) in patterns do
                if not matched then
                    let regex = Regex("^" + pattern, RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant)
                    let m = regex.Match(inputSubstring)
                    if m.Success then
                        matched <- true
                        newPosition <- position + m.Length
                        match action m.Value with
                        | Some token -> newTokens <- token :: newTokens
                        | None -> ()

            if matched then
                tokenizeRest newPosition newTokens inputStr
            else
                let errorChar = inputStr[position]
                failwithf "Unexpected character at position %d: '%c'" position errorChar

    tokenizeRest 0 [] input