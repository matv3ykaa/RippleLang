namespace RippleLang.AST

// Абстрактное синтаксическое дерево языка RippleLang
type Ident = string

// Литеральные значения
type Literal =
    | LInt of int
    | LFloat of float
    | LBool of bool
    | LString of string
    | LUnit // Аналог "unit", представление пустого значения

// Определение выражений
type Expr =
    | Literal of Literal
    | Variable of Ident
    | Lambda of Ident * Expr // Лямбда-выражение: \x -> body
    | Apply of Expr * Expr   // Применение функции: f x
    | Let of Ident * Expr * Expr  // Let-выражение: let x = value in body
    | LetRec of Ident * Expr * Expr // Рекурсивное let-выражение
    | If of Expr * Expr * Expr   // Условное выражение: if cond then expr1 else expr2
    | Op of Expr * Operator * Expr // Бинарные операции
    | List of Expr list         // Список: [a, b, c]
    | Tuple of Expr list        // Кортеж: (a, b, c)

// Операторы
and Operator =
    | Add | Sub | Mul | Div // Арифметические операции
    | Mod               // Остаток от деления
    | Eq | Neq          // Сравнение равенства
    | Gt | Lt | Gte | Lte // Сравнения
    | And | Or          // Логические операции
    | Cons             // Конструктор списка

// Определение программы как списка выражений
type Program = Expr list
