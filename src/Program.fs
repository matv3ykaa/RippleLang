module RippleLang.Program
open System
open RippleLang.Parser
open RippleLang.AST
open RippleLang.Value
open RippleLang.Environment
open RippleLang.StdLib
open RippleLang.Interpreter

// Функция для отображения AST в строку (для отладки)
let rec astToString expr =
    match expr with
    | Literal lit ->
        match lit with
        | LInt i -> string i
        | LFloat f -> string f
        | LBool b -> string b
        | LString s -> sprintf "\"%s\"" s
        | LUnit -> "()"
    | Variable name -> name
    | Lambda (param, body) -> sprintf "(\\%s -> %s)" param (astToString body)
    | Apply (func, arg) -> sprintf "(%s %s)" (astToString func) (astToString arg)
    | Let (name, value, body) ->
        sprintf "(let %s = %s in %s)" name (astToString value) (astToString body)
    | LetRec (name, value, body) ->
        sprintf "(let rec %s = %s in %s)" name (astToString value) (astToString body)
    | If (cond, thenExpr, elseExpr) ->
        sprintf "(if %s then %s else %s)"
            (astToString cond) (astToString thenExpr) (astToString elseExpr)
    | Op (left, op, right) ->
        let opStr =
            match op with
            | Add -> "+"
            | Sub -> "-"
            | Mul -> "*"
            | Div -> "/"
            | Mod -> "%"
            | Eq -> "=="
            | Neq -> "!="
            | Gt -> ">"
            | Lt -> "<"
            | Gte -> ">="
            | Lte -> "<="
            | And -> "&&"
            | Or -> "||"
            | Cons -> "::"
        sprintf "(%s %s %s)" (astToString left) opStr (astToString right)
    | List exprs ->
        let elements = exprs |> List.map astToString |> String.concat ", "
        sprintf "[%s]" elements
    | Tuple exprs ->
        let elements = exprs |> List.map astToString |> String.concat ", "
        sprintf "(%s)" elements

// Функция для запуска REPL (Read-Eval-Print Loop)
let runRepl() =
    let stdEnv = createStdEnv()
    printfn "RippleLang REPL - выход через Ctrl+C"
    printfn "----------------------------------------"

    let rec loop env =
        printf "> "
        let input = Console.ReadLine()
        if input = null then // Ctrl+C или EOF
            ()
        else if input.Trim() = "" then
            loop env // Пропускаем пустые строки
        else
            try
                let ast = parse input
                match ast with
                | [] -> 
                    loop env
                | expr::_ -> 
                    let result = evaluate expr env
                    printfn "=> %s" (valueToString result)
                    loop env
            with
            | ex ->
                printfn "Ошибка: %s" ex.Message
                loop env
    
    loop stdEnv

// Функция для выполнения RippleLang файлов
let runFile (path: string) =
    try
        let content = System.IO.File.ReadAllText(path)
        let ast = parse content
        let stdEnv = createStdEnv()
        let result = executeProgram ast stdEnv
        printfn "Результат: %s" (valueToString result)
        0 // Успешное завершение
    with
    | ex ->
        printfn "Ошибка: %s" ex.Message
        1

// Тестовая программа - факториал в RippleLang
let factorialProgram = """
let rec factorial = \n ->
  if n == 0 then
    1
  else
    n * factorial (n - 1)
  in
factorial 5
"""

// Тестовая программа - список Фибоначчи в RippleLang
let fibonacciProgram = """
let rec fib = \n ->
  if n <= 1 then
    n
  else
    fib (n - 1) + fib (n - 2)
  in
let rec makeList = \n ->
  if n < 0 then
    []
  else
    makeList (n - 1) ++ [fib n]
  in
makeList 10  // [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
"""

// Главная функция
[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        // Запускаем REPL если аргументы не указаны
        runRepl()
        0
    else if argv.[0] = "--test" then
        // Запускаем тесты
        printfn "Запуск тестовых программ RippleLang..."
        printfn "==================================="
        
        printfn "Тест 1: Факториал"
        printfn "-----------------"
        printfn "Программа:\n%s" factorialProgram
        try
            let factAst = parse factorialProgram
            let stdEnv = createStdEnv()
            let factResult = executeProgram factAst stdEnv
            printfn "Результат: %s\n" (valueToString factResult)
        with ex ->
            printfn "Ошибка: %s\n" ex.Message
        
        printfn "Тест 2: Числа Фибоначчи"
        printfn "---------------------"
        printfn "Программа:\n%s" fibonacciProgram
        try
            let fibAst = parse fibonacciProgram
            let stdEnv = createStdEnv()
            let fibResult = executeProgram fibAst stdEnv
            printfn "Результат: %s\n" (valueToString fibResult)
        with ex ->
            printfn "Ошибка: %s\n" ex.Message
        
        0
    else
        // Запускаем файл с исходным кодом
        runFile argv.[0]
        