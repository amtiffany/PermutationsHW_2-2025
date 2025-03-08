module Program 
    

// implement
    // 1. interpreter for an expression language for predicate logic
    // 2. construct a truth table for the given expression

type BinOp = 
    And | Or | Impl | Xor 
    member x.toString() =
        match x with 
        | And -> "and"
        | Or -> "or"
        | Impl -> "->"
        | Xor -> "xor"

type Expr = 
    | Symbol of string
    | BinOp of BinOp * Expr * Expr 
    | Not of Expr 

    static member (+) (x, y) = BinOp (Or, x,y)
    static member (*) (x,y) = BinOp (And, x,y)
    static member (-->) (x,y) = BinOp (Impl, x,y)
    static member (~-) (x) = Not x

    member x.toPrettyString() =
        match x with 
        | Symbol a -> $"`{a}`"
        | BinOp (op, a, b) -> $"({a.toPrettyString()} {op.toString()} {b.toPrettyString()})"
        | Not a -> "(- " + a.toPrettyString() + ")"
    


let A = Symbol "A"
let B = Symbol "B"
let nB = - B

let xx = B + (A * (- B)) --> B

printfn "%A" xx
printfn "%s" (xx.toPrettyString())

let rec allSymbols e = 
    seq {
        match e with 
        | Symbol s -> 
            yield s
        | BinOp(op, a, b)->
            yield! allSymbols a
            yield! allSymbols b
        | Not(a) -> 
            yield! allSymbols a
    }


type Environment = Map<string, bool>

let check (e:Environment) (syms:seq<string>) = Set.ofSeq <| Map.keys e = Set.ofSeq syms

let mapVar(exp, tf) =
    let rec mapVar (exp, e:Environment, var:Set<string>, tf:list<bool>):Environment*Set<string>*list<bool> =
        match exp with
        |Symbol(x) ->
            if (not (var.Contains(x))) then e.Add(x, tf.Head), var.Add(x), tf.Tail
            else e, var, tf
        |BinOp(_, x, y) ->
            let e1, var1, tf1 = mapVar (x, e, var, tf)
            let e2, var2, tf2 = mapVar(y, e1, var1, tf1)
            e2, var2, tf2
        |Not(x) -> mapVar (x, e, var, tf)
    let e:Environment = Map []
    let v = Set.empty
    let a, _, _ = mapVar(exp, e, v, tf)
    a


let rec eval (e:Environment) (expr:Expr) =
    // if check e (allSymbols expr) then
    if (true) then 
        match expr with
        | Not a -> not (eval e a)
        | Symbol(a) -> e[a]
        | BinOp(Or, x, y) ->
            if ((eval e x) = true) then true
            else if ((eval e y) = true) then true
            else false
        |BinOp(Xor, x, y) ->
            if ((eval e x ) = true) then
                if ((eval e y) = false) then true
                else false
            else if ((eval e y) = true) then true
            else false
        |BinOp(And, x, y) ->
            if((eval e x) = true) then
                if ((eval e y) = true) then true
                else false
            else false
        |BinOp(Impl, x, y) ->
            if ((eval e x) = false) then true
            else if ((eval e y) = true) then true
            else false
    else
        failwith "unbound symbols"
        
let permutations (alph:list<bool>, n) =
    match alph.Length, n with
    |0, _ -> failwith "no empty lists allowed"
    |_, 0 -> []
    |1, x -> [for i in 0..x -> [alph.Head]]
    |_, 1 -> [for i in alph -> [i]]
    |_, _ -> 
        let mutable n = n - 1
        let mutable output:list<list<bool>> = []
        for i in alph do
            for j in alph do
                output <- output@[[i; j]]
        
        let mutable output2 = output
        
        while n > 1 do
            output2 <- output
            output <- []
            for i in alph do
                for j in output2 do
                    output <- output@[i::j]
            output2 <- output
            n <- n - 1
        output
        
        
let truthTable exp =
    
    let rec countVar (exp, count, var:Set<string>):int*Set<string> =
        match exp with
        |Symbol(x) -> if (not (var.Contains(x))) then (count + 1, var.Add(x)) else (count, var)
        |BinOp(_, x, y) ->
            let count1, var1 = countVar (x, 0, var)
            let count2, var2 = countVar (y, 0, var1)
            count1 + count2 + count, var2
        |Not(x) -> countVar (x, count, var)
    
    
    let count, _ = countVar (exp, 0, Set.empty)
    printfn "%A" count
    let TFPerm = permutations ([true; false], count)
    printfn "%s" (exp.toPrettyString())
    for i in TFPerm do
        let e2 = mapVar (exp, i)
        printfn "%A" e2 
        printfn "%A" (eval e2 exp)


truthTable xx
        
        

// let rec truth (exp) =
//     match exp with
//     |Symbol("t") -> "t"
//     |Symbol("f") -> "f"
//     |Symbol(_) -> failwith "i only want t/f values and im too lazy to implement any other way"
//     |Not(x) ->
//         match truth(x) with
//         |"t" -> "f"
//         |"f" -> "t"
//         |_ -> failwith "only t/f values"
//     |BinOp(Or, x, y) ->
//         if (truth(x) = "t") then
//             "t"
//         else if (truth(y) = "t") then
//             "t"
//         else
//             "f"
//     |BinOp(Xor, x, y) ->
//         if (truth(x) = "t") then
//             if (truth(y) = "f") then
//                 "t"
//             else
//                 "f"
//         else if (truth(y) = "t") then
//             "t"
//         else
//             "f"
//     |BinOp(And, x, y) ->
//         if(truth(x) = "t") then
//             if (truth(y) = "t") then
//                 "t"
//             else
//                 "f"
//         else "f"
//     |BinOp(Impl, x, y) ->
//         if (truth(x) = "f") then
//             "t"
//         else if (truth(y) = "t") then
//             "t"
//         else
//             "f"
//             
// let truthTable exp =
//     
//     let rec countVar exp count =
//         match exp with
//         |Symbol(_) -> count + 1
//         |BinOp(_, x, y) -> (countVar x 0) + (countVar y 0) 
//         |Not(x) -> countVar x count
//     
//     let permutations (alph:list<string>, n) =
//         match alph.Length, n with
//         |0, _ -> failwith "no empty lists allowed"
//         |_, 0 -> []
//         |1, x -> [for i in 0..x -> alph.Head]
//         |_, 1 -> alph
//         |_, _ -> 
//             let mutable n = n - 1
//             let mutable output = []
//             for i in alph do
//                 for j in alph do
//                     output <- output@[i + j]
//             
//             let mutable output2 = output
//             
//             while n > 1 do
//                 output2 <- output
//                 output <- []
//                 for i in alph do
//                     for j in output2 do
//                         output <- output@[i + j]
//                 output2 <- output
//                 n <- n - 1
//             output
//     
//     let rec changeToTF (exp:Expr, TF:list<string>):Expr*list<string> = 
//         match exp with
//         |Symbol(_) -> (Symbol(TF.Head), [for i in 1..(TF.Length - 1) -> TF[i] ])
//         |Not(x) ->
//             let x1, l = changeToTF(x, TF)
//             Not(x1), l
//         |BinOp(o, x, y) ->
//             let newX, newTF = changeToTF(x, TF)
//             let newY, idc = changeToTF(y, [for i in newTF -> string(i)])
//             (BinOp(o, newX, newY), idc)
//     
//     let count = countVar exp 0
//     printfn "%A" count
//     let TFPerm = permutations (["t";"f"], count)
//     printfn "%A" TFPerm.Length
//     for i in TFPerm do
//         let newExp, idc = changeToTF (exp, (Seq.toList [for j in 0..(i.Length - 1) -> string(i[j])]))
//         printfn "%s" (newExp.toPrettyString())
//         printfn "%A" (truth(newExp))
        

//printfn "%A" (eval e xx)
        
    
            
    
    
            
        
            
        