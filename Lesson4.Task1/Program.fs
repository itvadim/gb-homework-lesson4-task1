// ==============================================
// Типы
// ==============================================

type Stack = StackContents of float list

/// Поместить значение в стек
let push x (StackContents contents) =   
    StackContents (x::contents)

/// Вытолкнуть значение из стека и вернуть его
/// и новый стек в виде пары
let pop (StackContents contents) = 
    match contents with 
    | top::rest -> 
        let newStack = StackContents rest
        (top,newStack)
    | [] -> 
        failwith "Стек пустой"

// вытолкнуть два верхних элемента
// применить к ним бинарную операцию
// положить результат в стек 
let binary mathFn stack = 
    let y,stack' = pop stack    
    let x,stack'' = pop stack'  
    let z = mathFn x y
    push z stack''      

// вытолкнуть вершину стека
// применить к ней унарную операцию
// положить результат в стек
let unary f stack = 
    let x,stack' = pop stack  
    push (f x) stack'         

/// Вытолкнуть и напечатать вершину стека
let show stack = 
    let x,_ = pop stack
    printfn "Вывод: %f" x
    stack  // продолжаем с тем же стеком

/// Дублировать вершину стека
let dup stack = 
    let x,s = pop stack  
    push x (push x s)   

/// Обменять два верхних значения местами
let swap stack = 
    let x,s = pop stack  
    let y,s' = pop s
    push y (push x s')   

/// Удалить вершину стека
let drop stack = 
    let _,s = pop stack  //вытолкнуть вершину стека
    s                    //вернуть всё остальное

// Костанты
// -------------------------------
let empty = StackContents []
let start  = empty

// Числа
// -------------------------------
let one = push 1.0
let two = push 2.0
let three = push 3.0
let four = push 4.0
let five = push 5.0

// Арифметические функции
// -------------------------------
let add = binary (+)
let sub = binary (-)
let mul = binary (*)
let div = binary (/)

let neg = unary (fun x -> -x)

// Вход в программу
// -------------------------------
[<EntryPoint>]
let main argv = // 1 2 + 4 * 3 +
    start 
    |> one
    |> two
    |> add
    |> four
    |> mul
    |> three
    |> add
    |> show
    |> ignore
    0