module Tests

open System 
open Xunit
open Program


[<Fact>]
let ``My test`` () =
    Assert.True(true)
    

[<Fact>]
let ``powerFunction powers of 2 test`` () =
    
    let TestPF2 = [2;4;8;16;32;64;128;256;]
    
    for i in 1..8 do

        let p2 = powerFunction 2 i
        
        Assert.Equal(TestPF2[i-1], p2)
        
[<Fact>]
let ``powerFunction powers of 3 test`` () =
    
    let TestPF3 = [ for i in 1..8 -> int(float(3)**i) ]
    
    for i in 1..8 do

        let p3 = powerFunction 3 i

        Assert.Equal(TestPF3[i-1], p3)
        
[<Fact>]
let ``permutations test 1`` () =
    let testPer1 = ["a";"b";"c";]
    let list1 = permutations(["a";"b";"c";], 1)
    for i in 0..(list1.Length - 1) do
        Assert.Equal(testPer1[i], list1[i])
      
[<Fact>]
let ``permutations test 2`` () =
    let testPer2 =["aa";"ab";"ac";"ba";"bb";"bc";"ca";"cb";"cc";]
    let list1 = permutations(["a";"b";"c";], 2)
    for i in 0..(list1.Length - 1) do
        Assert.True(testPer2[i] = list1[i])
        
[<Fact>]
let ``permutations test 3`` () =
    let testPer2=["aa";"ab";"ac";"ba";"bb";"bc";"ca";"cb";"cc";]
    let mutable testPer3 =[]
    for i in testPer2 do
        for j in ["a";"b";"c";] do
            testPer3 <- testPer3@[i + j]
            
    let list1 = permutations(["a";"b";"c";], 3)
    for i in 0..(list1.Length - 1) do
        Assert.True(testPer3[i] = list1[i])
        

[<Fact>]
let `` power set test 1`` () =
    let listPow1 = ["a"]
    let testPow1 = powerSet(["a"])
    Assert.Equal(listPow1[0], testPow1[0])
[<Fact>]
let `` power set test 2`` () =
    let listPow1 = ["a"; "ab"; "b"; "c"; "ac"; "abc"; "bc"]
    let testPow1 = powerSet(["a";"b";"c";])
    for i in  0..(list1.Length - 1) do 
        Assert.Equal(listPow1[i], testPow1[i])
[<Fact>]
let `` power set test 3`` () =
    let listPow1 = ["a"; "ab"; "b"; "c"; "ac"; "abc"; "bc"; "d"; "ad"; "abd"; "bd"; "cd"; "acd";"abcd"; "bcd"]
    let testPow1 = powerSet(["a";"b";"c";"d"])
    for i in  0..(list1.Length - 1) do 
        Assert.Equal(listPow1[i], testPow1[i])
    
