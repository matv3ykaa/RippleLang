module DropletLang.StdLib

open System
open System.IO
open DropletLang.Value
open DropletLang.Environment
open DropletLang.Interpreter

// Стандартная библиотека DropletLang

// Вспомогательные функции для вызова нативных функций
let callNative1 func arg =
    func [arg]

let callNative2 func arg1 arg2 =
    func [arg1; arg2]

// Функции для работы с числами
let numPrint = VNativeFunction("print", fun args ->
    match args with
    | [value] ->
        printfn "%s" (valueToString value)
        VUnit
    | _ -> failwith "print requires exactly one argument")

let numAdd = VNativeFunction("add", fun args ->
    match args with
    | [VInt a; VInt b] -> VInt (a + b)
    | [VFloat a; VFloat b] -> VFloat (a + b)
    | [VInt a; VFloat b] -> VFloat (float a + b)
    | [VFloat a; VInt b] -> VFloat (a + float b)
    | _ -> failwith "add requires two numeric arguments")

let numSub = VNativeFunction("sub", fun args ->
    match args with
    | [VInt a; VInt b] -> VInt (a - b)
    | [VFloat a; VFloat b] -> VFloat (a - b)
    | [VInt a; VFloat b] -> VFloat (float a - b)
    | [VFloat a; VInt b] -> VFloat (a - float b)
    | _ -> failwith "sub requires two numeric arguments")

let numMul = VNativeFunction("mul", fun args ->
    match args with
    | [VInt a; VInt b] -> VInt (a * b)
    | [VFloat a; VFloat b] -> VFloat (a * b)
    | [VInt a; VFloat b] -> VFloat (float a * b)
    | [VFloat a; VInt b] -> VFloat (a * float b)
    | _ -> failwith "mul requires two numeric arguments")

let numDiv = VNativeFunction("div", fun args ->
    match args with
    | [_; VInt 0] | [_; VFloat 0.0] ->
        failwith "Division by zero"
    | [VInt a; VInt b] -> VInt (a / b)
    | [VFloat a; VFloat b] -> VFloat (a / b)
    | [VInt a; VFloat b] -> VFloat (float a / b)
    | [VFloat a; VInt b] -> VFloat (a / float b)
    | _ -> failwith "div requires two numeric arguments")

let numMod = VNativeFunction("mod", fun args ->
    match args with
    | [_; VInt 0] ->
        failwith "Modulo by zero"
    | [VInt a; VInt b] -> VInt (a % b)
    | _ -> failwith "mod requires two integer arguments")

// Функции для работы со строками
let strLen = VNativeFunction("length", fun args ->
    match args with
    | [VString s] -> VInt s.Length
    | [VList l] -> VInt l.Length
    | _ -> failwith "length requires a string or list argument")

let strConcat = VNativeFunction("concat", fun args ->
    match args with
    | [VString a; VString b] -> VString (a + b)
    | [VList a; VList b] -> VList (a @ b)
    | _ -> failwith "concat requires two strings or two lists")

let strSubstr = VNativeFunction("substring", fun args ->
    match args with
    | [VString s; VInt start; VInt length] ->
        let start = max 0 start
        let length = max 0 (min (s.Length - start) length)
        VString (s.Substring(start, length))
    | _ -> failwith "substring requires a string and two integer arguments")

// Функции для работы с символами
let charToCode = VNativeFunction("_charToCode", fun args ->
    match args with
    | [VString s] when s.Length = 1 -> VInt (int s.[0])
    | _ -> failwith "_charToCode requires a single character string")

let codeToChar = VNativeFunction("_codeToChar", fun args ->
    match args with
    | [VInt code] -> VString (string (char code))
    | _ -> failwith "_codeToChar requires an integer code point")

// Функции для работы со списками
let listHead = VNativeFunction("head", fun args ->
    match args with
    | [VList (h::_)] -> h
    | [VList []] -> failwith "head: empty list"
    | _ -> failwith "head requires a list argument")

let listTail = VNativeFunction("tail", fun args ->
    match args with
    | [VList (_::t)] -> VList t
    | [VList []] -> failwith "tail: empty list"
    | _ -> failwith "tail requires a list argument")

let listCons = VNativeFunction("cons", fun args ->
    match args with
    | [item; VList l] -> VList (item :: l)
    | _ -> failwith "cons requires an item and a list")

let listMap = VNativeFunction("map", fun args ->
    match args with
    | [VFunction f; VList l] ->
        let result = l |> List.map (fun v ->
            match f with
            | UserFunction (env, param, body) ->
                let newEnv = extend param v env
                evaluate body newEnv
            | Closure (env, param, body) ->
                let newEnv = extend param v env
                evaluate body newEnv 
            | RecursiveFunction (env, funcName, param, body) ->
                let newEnv = extend param v env
                evaluate body newEnv)
        VList result
    | _ -> failwith "map requires a function and a list")

let listFilter = VNativeFunction("filter", fun args ->
    match args with
    | [VFunction f; VList l] ->
        let result = l |> List.filter (fun v ->
            match f with
            | UserFunction (env, param, body) ->
                let newEnv = extend param v env
                toBool (evaluate body newEnv)
            | Closure (env, param, body) ->
                let newEnv = extend param v env
                toBool (evaluate body newEnv)
            | RecursiveFunction (env, funcName, param, body) ->
                let newEnv = extend param v env
                toBool (evaluate body newEnv))
        VList result
    | _ -> failwith "filter requires a function and a list")

let listFold = VNativeFunction("fold", fun args ->
    match args with
    | [VFunction f; acc; VList l] ->
        l |> List.fold (fun acc' v ->
            match f with
            | UserFunction (env, param, body) ->
                let newEnv = extend param v env
                let newEnv' = extend "acc" acc' newEnv
                evaluate body newEnv'
            | Closure (env, param, body) ->
                let newEnv = extend param v env
                let newEnv' = extend "acc" acc' newEnv
                evaluate body newEnv'
            | RecursiveFunction (env, funcName, param, body) ->
                let newEnv = extend param v env
                let newEnv' = extend "acc" acc' newEnv
                evaluate body newEnv') acc
    | _ -> failwith "fold requires a function, accumulator, and a list")

// Функции для работы с вводом/выводом
let ioPrint = VNativeFunction("_print", fun args ->
    match args with
    | [value] ->
        printfn "%s" (valueToString value)
        VUnit
    | _ -> failwith "print requires exactly one argument")

let ioInput = VNativeFunction("_input", fun args ->
    match args with
    | [] ->
        let input = Console.ReadLine()
        VString input
    | [VString prompt] ->
        printf "%s" prompt
        let input = Console.ReadLine()
        VString input
    | _ -> failwith "input requires zero or one string argument")

// Функции для работы с файлами
let fileRead = VNativeFunction("_readFile", fun args ->
    match args with
    | [VString path] -> 
        try
            let content = File.ReadAllText(path)
            VString content
        with
        | ex -> failwithf "Error reading file: %s" ex.Message
    | _ -> failwith "_readFile requires a file path")

let fileWrite = VNativeFunction("_writeFile", fun args ->
    match args with
    | [VString path; VString content] ->
        try
            File.WriteAllText(path, content)
            VUnit
        with
        | ex -> failwithf "Error writing to file: %s" ex.Message
    | _ -> failwith "_writeFile requires a file path and content")

let fileAppend = VNativeFunction("_appendFile", fun args ->
    match args with
    | [VString path; VString content] ->
        try
            File.AppendAllText(path, content)
            VUnit
        with
        | ex -> failwithf "Error appending to file: %s" ex.Message
    | _ -> failwith "_appendFile requires a file path and content")

let fileExists = VNativeFunction("_fileExists", fun args ->
    match args with
    | [VString path] -> 
        VBool (File.Exists(path))
    | _ -> failwith "_fileExists requires a file path")

// Функции для конвертации типов
let toInt = VNativeFunction("_toInt", fun args ->
    match args with
    | [value] ->
        match tryToInt value with
        | Some i -> VInt i
        | None -> failwithf "Cannot convert %s to integer" (valueToString value)
    | _ -> failwith "toInt requires exactly one argument")

let toFloat = VNativeFunction("_toFloat", fun args ->
    match args with
    | [value] ->
        match tryToFloat value with
        | Some f -> VFloat f
        | None -> failwithf "Cannot convert %s to float" (valueToString value)
    | _ -> failwith "toFloat requires exactly one argument")

let toString = VNativeFunction("_toString", fun args ->
    match args with
    | [value] -> VString (valueToString value)
    | _ -> failwith "toString requires exactly one argument")

// Математические функции
let mathSin = VNativeFunction("sin", fun args ->
    match args with
    | [VFloat x] -> VFloat (Math.Sin(x))
    | [VInt x] -> VFloat (Math.Sin(float x))
    | _ -> failwith "sin requires a numeric argument")

let mathCos = VNativeFunction("cos", fun args ->
    match args with
    | [VFloat x] -> VFloat (Math.Cos(x))
    | [VInt x] -> VFloat (Math.Cos(float x))
    | _ -> failwith "cos requires a numeric argument")

let mathSqrt = VNativeFunction("sqrt", fun args ->
    match args with
    | [VFloat x] when x >= 0.0 -> VFloat (Math.Sqrt(x))
    | [VInt x] when x >= 0 -> VFloat (Math.Sqrt(float x))
    | _ -> failwith "sqrt requires a non-negative numeric argument")

let mathPow = VNativeFunction("pow", fun args ->
    match args with
    | [VFloat x; VFloat y] -> VFloat (Math.Pow(x, y))
    | [VInt x; VInt y] -> VFloat (Math.Pow(float x, float y))
    | [VFloat x; VInt y] -> VFloat (Math.Pow(x, float y))
    | [VInt x; VFloat y] -> VFloat (Math.Pow(float x, y))
    | _ -> failwith "pow requires two numeric arguments")

// Создание стандартного окружения
let createStdEnv() : Environment =
    emptyEnv
    |> extendMany [
        // Математические операции
        "add", numAdd
        "sub", numSub
        "mul", numMul
        "div", numDiv
        "mod", numMod
        
        // Встроенные математические функции
        "sin", mathSin
        "cos", mathCos
        "sqrt", mathSqrt
        "pow", mathPow

        // Строковые операции
        "length", strLen
        "concat", strConcat
        "substring", strSubstr
        "_charToCode", charToCode
        "_codeToChar", codeToChar

        // Списковые операции
        "head", listHead
        "tail", listTail
        "cons", listCons
        "map", listMap
        "filter", listFilter
        "fold", listFold

        // Ввод/вывод
        "_print", ioPrint
        "_input", ioInput
        
        // Файловые операции
        "_readFile", fileRead
        "_writeFile", fileWrite
        "_appendFile", fileAppend
        "_fileExists", fileExists

        // Конвертация типов
        "_toInt", toInt
        "_toFloat", toFloat
        "_toString", toString

        // Константы
        "pi", VFloat Math.PI
        "e", VFloat Math.E
        "true", VBool true
        "false", VBool false
        "empty", VList []
    ]