module RippleLang.Value

open RippleLang.AST

// Представление значений в RippleLang
type Value =
    | VInt of int
    | VFloat of float
    | VBool of bool
    | VString of string
    | VUnit
    | VFunction of Function
    | VList of Value list
    | VTuple of Value list
    | VNativeFunction of string * (Value list -> Value)

// Представление функций
and Function =
    | UserFunction of Environment * Ident * Expr
    | Closure of Environment * Ident * Expr
    | RecursiveFunction of Environment * Ident * Ident * Expr

// Окружение для хранения переменных
and Environment = Map<string, Value>

// Преобразование значения в строку для вывода
let rec valueToString value =
    match value with
    | VInt i -> string i
    | VFloat f -> string f
    | VBool b -> string b
    | VString s -> sprintf "\"%s\"" s
    | VUnit -> "()"
    | VFunction _ -> "<function>"
    | VNativeFunction (name, _) -> sprintf "<native function: %s>" name
    | VList values ->
        let elements = values |> List.map valueToString |> String.concat ", "
        sprintf "[%s]" elements
    | VTuple values ->
        let elements = values |> List.map valueToString |> String.concat ", "
        sprintf "(%s)" elements

// Преобразование литерала AST в значение
let literalToValue (literal: Literal) : Value =
    match literal with
    | LInt i -> VInt i
    | LFloat f -> VFloat f
    | LBool b -> VBool b
    | LString s -> VString s
    | LUnit -> VUnit

// Функции преобразования типов
let toBool = function
    | VBool b -> b
    | VInt 0 -> false
    | VInt _ -> true
    | VFloat 0.0 -> false
    | VFloat _ -> true
    | VString "" -> false
    | VString _ -> true
    | VList [] -> false
    | VList _ -> true
    | VUnit -> false
    | _ -> true

let tryToInt = function
    | VInt i -> Some i
    | VFloat f -> Some (int f)
    | VBool true -> Some 1
    | VBool false -> Some 0
    | VString s ->
        match System.Int32.TryParse(s) with
        | true, v -> Some v
        | _ -> None
    | _ -> None

let tryToFloat = function
    | VInt i -> Some (float i)
    | VFloat f -> Some f
    | VBool true -> Some 1.0
    | VBool false -> Some 0.0
    | VString s ->
        match System.Double.TryParse(s) with
        | true, v -> Some v
        | _ -> None
    | _ -> None
