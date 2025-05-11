module RippleLang.Environment

open RippleLang.Value

// Создание пустого окружения
let emptyEnv : Environment = Map.empty

// Получение значения переменной из окружения
let lookup (name: string) (env: Environment) : Value option =
    match Map.tryFind name env with
    | Some value -> Some value
    | None -> None

// Добавление переменной в окружение
let extend (name: string) (value: Value) (env: Environment) : Environment =
    Map.add name value env

// Расширение окружения несколькими переменными
let extendMany (bindings: (string * Value) list) (env: Environment) : Environment =
    List.fold (fun e (name, value) -> extend name value e) env bindings

// Создание копии окружения
let copy (env: Environment) : Environment = env
