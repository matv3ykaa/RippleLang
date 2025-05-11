module RippleLang.StdLib

open System
open RippleLang.Value
open RippleLang.Environment
open RippleLang.Interpreter

// Стандартная библиотека RippleLang

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
let ioPrint = VNativeFunction("print", fun args ->
    match args with
    | [value] ->
        printfn "%s" (valueToString value)
        VUnit
    | _ -> failwith "print requires exactly one argument")

let ioInput = VNativeFunction("input", fun args ->
    match args with
    | [] ->
        let input = Console.ReadLine()
        VString input
    | [VString prompt] ->
        printf "%s" prompt
        let input = Console.ReadLine()
        VString input
    | _ -> failwith "input requires zero or one string argument")

// Функции для конвертации типов
let toInt = VNativeFunction("toInt", fun args ->
    match args with
    | [value] ->
        match tryToInt value with
        | Some i -> VInt i
        | None -> failwithf "Cannot convert %s to integer" (valueToString value)
    | _ -> failwith "toInt requires exactly one argument")

let toFloat = VNativeFunction("toFloat", fun args ->
    match args with
    | [value] ->
        match tryToFloat value with
        | Some f -> VFloat f
        | None -> failwithf "Cannot convert %s to float" (valueToString value)
    | _ -> failwith "toFloat requires exactly one argument")

let toString = VNativeFunction("toString", fun args ->
    match args with
    | [value] -> VString (valueToString value)
    | _ -> failwith "toString requires exactly one argument")

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

        // Строковые операции
        "length", strLen
        "concat", strConcat
        "substring", strSubstr

        // Списковые операции
        "head", listHead
        "tail", listTail
        "cons", listCons
        "map", listMap
        "filter", listFilter
        "fold", listFold

        // Ввод/вывод
        "print", ioPrint
        "input", ioInput

        // Конвертация типов
        "toInt", toInt
        "toFloat", toFloat
        "toString", toString

        // Константы
        "pi", VFloat Math.PI
        "e", VFloat Math.E
        "true", VBool true
        "false", VBool false
        "empty", VList []
    ]
