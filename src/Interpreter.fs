module RippleLang.Interpreter

open RippleLang.AST
open RippleLang.Value
open RippleLang.Environment

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

// Функция для вычисления выражений
let rec evaluate (expr: Expr) (env: Environment) : Value =
    match expr with
    | Literal lit -> literalToValue lit

    | Variable name ->
        match lookup name env with
        | Some value -> value
        | None -> failwithf "Undefined variable: %s" name

    | Lambda (param, body) ->
        VFunction (Closure (env, param, body))

    | Apply (func, arg) ->
        let funcValue = evaluate func env
        let argValue = evaluate arg env  // Evaluate the argument first

        match funcValue with
        | VFunction f ->
            match f with
            | RecursiveFunction (closureEnv, funcName, param, body) ->
                // Обновляем окружение, включая текущий экземпляр функции
                let updatedEnv = closureEnv |> extend funcName funcValue
                let newEnv = updatedEnv |> extend param argValue
                evaluate body newEnv
            | UserFunction (closureEnv, param, body) ->
                let newEnv = extend param argValue closureEnv
                evaluate body newEnv
            | Closure (closureEnv, param, body) ->
                let newEnv = extend param argValue closureEnv
                evaluate body newEnv

        | _ ->
            failwithf "Cannot apply non-function value: %s" (valueToString funcValue)

    | Let (name, valueExpr, bodyExpr) ->
        let value = evaluate valueExpr env
        let newEnv = extend name value env
        evaluate bodyExpr newEnv

    | LetRec (name, valueExpr, bodyExpr) ->
        match valueExpr with
        | Lambda (param, lambdaBody) ->
            // Create recursive function
            let funcRef = ref VUnit  // Temporary placeholder
            let tempEnv = extend name (!funcRef) env
            let recursiveFunc = VFunction (RecursiveFunction (tempEnv, name, param, lambdaBody))
            funcRef := recursiveFunc  // Update the reference
            let newEnv = extend name recursiveFunc env
            evaluate bodyExpr newEnv

        | _ ->
            failwith "let rec requires a lambda expression"

    | If (condExpr, thenExpr, elseExpr) ->
        let condition = evaluate condExpr env
        if toBool condition then
            evaluate thenExpr env
        else
            evaluate elseExpr env

    | Op (left, op, right) ->
        let leftValue = evaluate left env

        // Специальная обработка для логических "&&" и "||" с коротким замыканием
        match op with
        | And ->
            if not (toBool leftValue) then
                VBool false  // Если левая часть false, сразу возвращаем false без вычисления правой
            else
                let rightValue = evaluate right env
                VBool (toBool rightValue)

        | Or ->
            if toBool leftValue then
                VBool true   // Если левая часть true, сразу возвращаем true без вычисления правой
            else
                let rightValue = evaluate right env
                VBool (toBool rightValue)

        | _ ->
            // Обычное вычисление бинарной операции
            let rightValue = evaluate right env
            evaluateOp leftValue op rightValue

    | List exprs ->
        VList (exprs |> List.map (fun e -> evaluate e env))

    | Tuple exprs ->
        VTuple (exprs |> List.map (fun e -> evaluate e env))

// Выполнение программы (списка выражений)
let executeProgram (program: Program) (env: Environment) : Value =
    match program with
    | [] -> VUnit  // Пустая программа возвращает Unit
    | expressions ->
        // Выполняем все выражения последовательно и возвращаем результат последнего
        expressions
        |> List.fold (fun _ expr -> evaluate expr env) VUnit
