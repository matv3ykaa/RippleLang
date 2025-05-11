module DropletLang.Interpreter

open DropletLang.AST
open DropletLang.Value
open DropletLang.Environment
open System.IO

// Функция для вычисления бинарных операций
let rec evaluateOp left op right =
    match op with
    | Add ->
        match left, right with
        | VInt l, VInt r -> VInt (l + r)
        | VFloat l, VFloat r -> VFloat (l + r)
        | VInt l, VFloat r -> VFloat (float l + r)
        | VFloat l, VInt r -> VFloat (l + float r)
        | VString l, VString r -> VString (l + r)
        | VList l, VList r -> VList (l @ r)
        | _ -> failwithf "Cannot add %s and %s" (valueToString left) (valueToString right)

    | Sub ->
        match left, right with
        | VInt l, VInt r -> VInt (l - r)
        | VFloat l, VFloat r -> VFloat (l - r)
        | VInt l, VFloat r -> VFloat (float l - r)
        | VFloat l, VInt r -> VFloat (l - float r)
        | _ -> failwithf "Cannot subtract %s from %s" (valueToString right) (valueToString left)

    | Mul ->
        match left, right with
        | VInt l, VInt r -> VInt (l * r)
        | VFloat l, VFloat r -> VFloat (l * r)
        | VInt l, VFloat r -> VFloat (float l * r)
        | VFloat l, VInt r -> VFloat (l * float r)
        | VString l, VInt r -> VString (String.replicate r l)
        | VInt l, VString r -> VString (String.replicate l r)
        | VList l, VInt r -> VList (List.collect (fun x -> List.replicate r x) l)
        | _ -> failwithf "Cannot multiply %s and %s" (valueToString left) (valueToString right)

    | Div ->
        match left, right with
        | _, (VInt 0 | VFloat 0.0) -> failwith "Division by zero"
        | VInt l, VInt r -> VInt (l / r)
        | VFloat l, VFloat r -> VFloat (l / r)
        | VInt l, VFloat r -> VFloat (float l / r)
        | VFloat l, VInt r -> VFloat (l / float r)
        | _ -> failwithf "Cannot divide %s by %s" (valueToString left) (valueToString right)

    | Mod ->
        match left, right with
        | _, (VInt 0 | VFloat 0.0) -> failwith "Modulo by zero"
        | VInt l, VInt r -> VInt (l % r)
        | _ -> failwithf "Modulo requires integer operands, got %s %% %s" (valueToString left) (valueToString right)

    | Eq ->
        match left, right with
        | VInt l, VInt r -> VBool (l = r)
        | VFloat l, VFloat r -> VBool (l = r)
        | VBool l, VBool r -> VBool (l = r)
        | VString l, VString r -> VBool (l = r)
        | VUnit, VUnit -> VBool true
        | VList l, VList r ->
            if l.Length <> r.Length then VBool false
            else VBool (List.forall2 (fun a b -> toBool (evaluateOp a Eq b)) l r)
        | VTuple l, VTuple r ->
            if l.Length <> r.Length then VBool false
            else VBool (List.forall2 (fun a b -> toBool (evaluateOp a Eq b)) l r)
        | _ -> VBool false

    | Neq ->
        match evaluateOp left Eq right with
        | VBool b -> VBool (not b)
        | _ -> failwith "Unexpected error in not-equal comparison"

    | Lt ->
        match left, right with
        | VInt l, VInt r -> VBool (l < r)
        | VFloat l, VFloat r -> VBool (l < r)
        | VInt l, VFloat r -> VBool (float l < r)
        | VFloat l, VInt r -> VBool (l < float r)
        | VString l, VString r -> VBool (l < r)
        | _ -> failwithf "Cannot compare %s < %s" (valueToString left) (valueToString right)

    | Lte ->
        match left, right with
        | VInt l, VInt r -> VBool (l <= r)
        | VFloat l, VFloat r -> VBool (l <= r)
        | VInt l, VFloat r -> VBool (float l <= r)
        | VFloat l, VInt r -> VBool (l <= float r)
        | VString l, VString r -> VBool (l <= r)
        | _ -> failwithf "Cannot compare %s <= %s" (valueToString left) (valueToString right)

    | Gt ->
        match left, right with
        | VInt l, VInt r -> VBool (l > r)
        | VFloat l, VFloat r -> VBool (l > r)
        | VInt l, VFloat r -> VBool (float l > r)
        | VFloat l, VInt r -> VBool (l > float r)
        | VString l, VString r -> VBool (l > r)
        | _ -> failwithf "Cannot compare %s > %s" (valueToString left) (valueToString right)

    | Gte ->
        match left, right with
        | VInt l, VInt r -> VBool (l >= r)
        | VFloat l, VFloat r -> VBool (l >= r)
        | VInt l, VFloat r -> VBool (float l >= r)
        | VFloat l, VInt r -> VBool (l >= float r)
        | VString l, VString r -> VBool (l >= r)
        | _ -> failwithf "Cannot compare %s >= %s" (valueToString left) (valueToString right)

    | And ->
        VBool (toBool left && toBool right)

    | Or ->
        VBool (toBool left || toBool right)

    | Cons ->
        match right with
        | VList items -> VList (left :: items)
        | _ -> failwithf "Cannot cons %s to non-list %s" (valueToString left) (valueToString right)

// Получение модуля для импорта
let importModule (path: string) (baseDir: string) (stdEnv: Environment) =
    let fullPath = 
        if Path.IsPathRooted(path) then 
            path 
        else 
            Path.Combine(baseDir, path)
    
    if not (fullPath.EndsWith(".drop")) then
        failwithf "Expected .drop file for import: %s" path
    
    if not (File.Exists(fullPath)) then
        failwithf "Module not found: %s" path
    
    let moduleContent = File.ReadAllText(fullPath)
    let moduleAst = parse moduleContent
    executeProgram moduleAst stdEnv |> ignore
    stdEnv

// Функция для вычисления выражений
and evaluate (expr: Expr) (env: Environment) (baseDir: string) : Value =
    match expr with
    | Literal lit -> literalToValue lit

    | Variable name ->
        match lookup name env with
        | Some value -> value
        | None -> failwithf "Undefined variable: %s" name

    | Flow (param, body) ->
        VFunction (Closure (env, param, body))

    | Apply (func, arg) ->
        let funcValue = evaluate func env baseDir
        let argValue = evaluate arg env baseDir

        match funcValue with
        | VFunction f ->
            match f with
            | RecursiveFunction (closureEnv, funcName, param, body) ->
                // Создаем копию окружения с функцией, привязанной к ее имени
                let recEnv = extend funcName funcValue closureEnv
                // Добавляем аргумент в окружение
                let newEnv = extend param argValue recEnv
                evaluate body newEnv baseDir
            | UserFunction (closureEnv, param, body) ->
                let newEnv = extend param argValue closureEnv
                evaluate body newEnv baseDir
            | Closure (closureEnv, param, body) ->
                let newEnv = extend param argValue closureEnv
                evaluate body newEnv baseDir
        | VNativeFunction (_, impl) ->
            impl [argValue]
        | _ ->
            failwithf "Cannot apply non-function value: %s" (valueToString funcValue)

    | Let (name, valueExpr, bodyExpr) ->
        let value = evaluate valueExpr env baseDir
        let newEnv = extend name value env
        evaluate bodyExpr newEnv baseDir

    | LetRec (name, valueExpr, bodyExpr) ->
        match valueExpr with
        | Flow (param, lambdaBody) ->
            // Создаем рекурсивную функцию
            let recFunc = VFunction (RecursiveFunction (env, name, param, lambdaBody))
            let newEnv = extend name recFunc env
            evaluate bodyExpr newEnv baseDir
        | _ ->
            failwith "let rec requires a flow expression"

    | When (condExpr, thenExpr, elseExpr) ->
        let condition = evaluate condExpr env baseDir
        if toBool condition then
            evaluate thenExpr env baseDir
        else
            evaluate elseExpr env baseDir

    | Drip (initExpr, condExpr, stepExpr, bodyExpr) ->
        // Инициализация
        let initValue = evaluate initExpr env baseDir
        
        // Создаем новое окружение с переменной цикла
        let loopVar = "_drip_counter"
        let loopEnv = extend loopVar initValue env
        
        // Рекурсивная функция для выполнения цикла
        let rec executeLoop env =
            // Проверяем условие
            let condValue = evaluate condExpr env baseDir
            if toBool condValue then
                // Выполняем тело цикла
                let _ = evaluate bodyExpr env baseDir
                
                // Выполняем шаг
                let stepValue = evaluate stepExpr env baseDir
                
                // Обновляем переменную цикла
                let newEnv = extend loopVar stepValue env
                
                // Следующая итерация
                executeLoop newEnv
            else
                VUnit
                
        executeLoop loopEnv

    | Op (left, op, right) ->
        let leftValue = evaluate left env baseDir

        // Короткое замыкание для логических операторов
        match op with
        | And ->
            if not (toBool leftValue) then
                VBool false
            else
                let rightValue = evaluate right env baseDir
                VBool (toBool rightValue)
        | Or ->
            if toBool leftValue then
                VBool true
            else
                let rightValue = evaluate right env baseDir
                VBool (toBool rightValue)
        | _ ->
            // Обычное вычисление для других операторов
            let rightValue = evaluate right env baseDir
            evaluateOp leftValue op rightValue

    | List exprs ->
        VList (exprs |> List.map (fun e -> evaluate e env baseDir))

    | Tuple exprs ->
        VTuple (exprs |> List.map (fun e -> evaluate e env baseDir))
        
    | Import path ->
        // Импортируем модуль и возвращаем VUnit
        let _ = importModule path baseDir env
        VUnit

// Вызов парсера
and parse source =
    DropletLang.Parser.parse source 

// Выполнение программы (списка выражений)
and executeProgram (program: Program) (env: Environment) : Value =
    let baseDir = Directory.GetCurrentDirectory()
    match program with
    | [] -> VUnit  // Пустая программа возвращает Unit
    | expressions ->
        // Выполняем все выражения последовательно и возвращаем результат последнего
        expressions
        |> List.fold (fun _ expr -> evaluate expr env baseDir) VUnit