module Tests
open Program

open System
open Xunit

    
    
[<Fact>]
let ``eval test 1`` () =
    let ex:Expr = Symbol("A")
    
    let en = mapVar(ex, [true])
    let ev = eval en ex
    Assert.True(ev)
    
    let en = mapVar(ex, [false])
    let ev = eval en ex
    Assert.False(ev)
    
[<Fact>]
let ``eval test 2`` () =
    let ex:Expr = BinOp(And, Symbol("A"), Symbol("B"))
    let tf = [[true; true]; [true; false]; [false; true]; [false; false]]
    for i in tf do
        let en = mapVar(ex, i)
        let ev = eval en ex
        let areEqual = (ev = (i[0] && i[1]))
        Assert.True(areEqual)
[<Fact>]
let ``eval test 3`` () =
    let ex:Expr = BinOp(Or, Symbol("A"), Symbol("B"))
    let tf = [[true; true]; [true; false]; [false; true]; [false; false]]
    for i in tf do
        let en = mapVar(ex, i)
        let ev = eval en ex
        let areEqual = (ev = (i[0] || i[1]))
        Assert.True(areEqual)
[<Fact>]
let ``eval test 4`` () =
    let ex:Expr = BinOp(Xor, Symbol("A"), Symbol("B"))
    let tf = [[true; true]; [true; false]; [false; true]; [false; false]]
    for i in tf do
        let en = mapVar(ex, i)
        let ev = eval en ex
        let areEqual = (ev = (i[0] <> i[1]))
        Assert.True(areEqual)
[<Fact>]
let ``eval test 5`` () =
    let ex:Expr = BinOp(Impl, Symbol("A"), Symbol("B"))
    let tf = [[true; true]; [true; false]; [false; true]; [false; false]]
    for i in tf do
        let en = mapVar(ex, i)
        let ev = eval en ex
        let areEqual = (ev = (not i[0] || i[1]))
        Assert.True(areEqual)
        
[<Fact>]
let ``eval test 6`` () =
    let ex:Expr = BinOp(And, BinOp(And, Symbol("A"), Symbol("B")), Symbol("C"))
    let tf = permutations([true; false], 3)
    for i in tf do
        let en = mapVar(ex, i)
        let ev = eval en ex
        let areEqual = (ev = (i[0] && i[1] && i[2]))
        Assert.True(areEqual)
        
[<Fact>]
let ``eval test 7`` () =
    let ex:Expr = BinOp(Or, BinOp(And, Symbol("A"), Symbol("B")), Symbol("C"))
    let tf = permutations([true; false], 3)
    for i in tf do
        let en = mapVar(ex, i)
        let ev = eval en ex
        let areEqual = (ev = ((i[0] && i[1]) || i[2]))
        Assert.True(areEqual)
        
[<Fact>]
let ``eval test 8`` () =
    let ex:Expr = BinOp(Xor, BinOp(And, Symbol("A"), Symbol("B")), Symbol("C"))
    let tf = permutations([true; false], 3)
    for i in tf do
        let en = mapVar(ex, i)
        let ev = eval en ex
        let areEqual = (ev = ((i[0] && i[1]) <> i[2]))
        Assert.True(areEqual)
        
[<Fact>]
let ``eval test 9`` () =
    let ex:Expr = BinOp(Impl, BinOp(And, Symbol("A"), Symbol("B")), Symbol("C"))
    let tf = permutations([true; false], 3)
    for i in tf do
        let en = mapVar(ex, i)
        let ev = eval en ex
        let areEqual = (ev = (not (i[0] && i[1]) || i[2]))
        Assert.True(areEqual)
        
        
[<Fact>]
let ``eval test 10`` () =
    let ex:Expr = BinOp(And, BinOp(And, Symbol("A"), Symbol("B")), Symbol("A"))
    let tf = permutations([true; false], 2)
    for i in tf do
        let en = mapVar(ex, i)
        let ev = eval en ex
        let areEqual = (ev = (i[0] && i[1] && i[0]))
        Assert.True(areEqual)
        
[<Fact>]
let ``eval test 11`` () =
    let ex:Expr = BinOp(Or, BinOp(And, Symbol("A"), Symbol("B")), Symbol("A"))
    let tf = permutations([true; false], 2)
    for i in tf do
        let en = mapVar(ex, i)
        let ev = eval en ex
        let areEqual = (ev = ((i[0] && i[1]) || i[0]))
        Assert.True(areEqual)
        
[<Fact>]
let ``eval test 12`` () =
    let ex:Expr = BinOp(Xor, BinOp(And, Symbol("A"), Symbol("B")), Symbol("A"))
    let tf = permutations([true; false], 2)
    for i in tf do
        let en = mapVar(ex, i)
        let ev = eval en ex
        let areEqual = (ev = ((i[0] && i[1]) <> i[0]))
        Assert.True(areEqual)
        
[<Fact>]
let ``eval test 13`` () =
    let ex:Expr = BinOp(Impl, BinOp(And, Symbol("A"), Symbol("B")), Symbol("A"))
    let tf = permutations([true; false], 2)
    for i in tf do
        let en = mapVar(ex, i)
        let ev = eval en ex
        let areEqual = (ev = (not (i[0] && i[1]) || i[0]))
        Assert.True(areEqual)
        
[<Fact>]
let ``eval test 14`` () =
    let ex:Expr = BinOp(And, Symbol("A"), Symbol("A"))
    let tf = permutations([true; false], 2)
    for i in tf do
        let en = mapVar(ex, i)
        let ev = eval en ex
        let areEqual = (ev = (i[0] && i[1]))
        Assert.True(areEqual)
[<Fact>]
let ``eval test 15`` () =
    let ex:Expr = BinOp(And, Symbol("A"), Symbol("B"))
    let tf = permutations([true; false], 3) //num in tf shouldnt matter
    for i in tf do
        let en = mapVar(ex, i)
        let ev = eval en ex
        let areEqual = (ev =  (i[0] && i[1]))
        Assert.True(areEqual)