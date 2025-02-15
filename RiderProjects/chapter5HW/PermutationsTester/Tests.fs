module Tests

open System 
open Xunit
open Program


[<Fact>]
let ``My test`` () =
    Assert.True(true)
    
[<Fact>]
let ``powerFunction powers of 0 test`` () =
    for i in 0..8 do
        let p0 = powerFunction 0 i
        Assert.Equal(0, p0)

[<Fact>]
let ``powerFunction powers of 1 test`` () =
    for i in 1..8 do
        let p0 = powerFunction 1 i
        Assert.Equal(1, p0)

[<Fact>]
let ``powerFunction exponents of 0 test`` () =
    for i in 1..8 do
        let p0 = powerFunction i 0
        Assert.Equal(1, p0)

[<Fact>]
let ``powerFunction exponents of 1 test`` () =
    for i in 1..8 do
        let p0 = powerFunction i 1
        Assert.Equal(i, p0)

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
let ``powerFunction powers of 4 test`` () =
    
    let TestPF3 = [ for i in 1..8 -> int(float(4)**i) ]
    
    for i in 1..8 do

        let p4 = powerFunction 4 i

        Assert.Equal(TestPF3[i-1], p4)

//permutations edge cases
[<Fact>]
let ``permutations test 00`` () =
    let testPer1 = ["a";]
    let list1 = permutations(["a";], 1)
    Assert.Equal(testPer1[0], list1[0])

[<Fact>]
let ``permutations test 01`` () =
    let list1 = permutations(["a";], 0)
    Assert.True(list1.IsEmpty)

[<Fact>]
let ``permutations test 04`` () =
    let list1 = permutations(["a";"b";"c"], 0)
    Assert.True(list1.IsEmpty)
    
//permutations tests (no duplicate items)
[<Fact>]
let ``permutations test 1`` () =
    let testPer1 = ["a";"b";]
    let list1 = permutations(["a";"b";], 1)
    for i in 0..(list1.Length - 1) do
        Assert.Equal(testPer1[i], list1[i])
        
[<Fact>]
let ``permutations test 2`` () =
    let testPer1 = ["aa";"ab";"ba";"bb"]
    let list1 = permutations(["a";"b";], 2)
    for i in 0..(list1.Length - 1) do
        Assert.Equal(testPer1[i], list1[i])
        
[<Fact>]
let ``permutations test 3`` () =
    let testPer1 = ["a";"b";"c";]
    let list1 = permutations(["a";"b";"c";], 1)
    for i in 0..(list1.Length - 1) do
        Assert.Equal(testPer1[i], list1[i])
[<Fact>]
let ``permutations test 4`` () =
    let testPer2 =["aa";"ab";"ac";"ba";"bb";"bc";"ca";"cb";"cc";]
    let list1 = permutations(["a";"b";"c";], 2)
    for i in 0..(list1.Length - 1) do
        Assert.True(testPer2[i] = list1[i])
        
[<Fact>]
let ``permutations test 5`` () =
    let testPer2=["aa";"ab";"ac";"ba";"bb";"bc";"ca";"cb";"cc";]
    let mutable testPer3 =[]
    for i in testPer2 do
        for j in ["a";"b";"c";] do
            testPer3 <- testPer3@[i + j]
            
    let list1 = permutations(["a";"b";"c";], 3)
    for i in 0..(list1.Length - 1) do
        Assert.True(testPer3[i] = list1[i])

// permutations tests (duplicate items)
// note: the way i wrote it doesnt check for duplicate items so the "a" in testPer1[0]
// is treated as a distinct element from the "a" in testPer1[1]
let ``permutations test 6`` () =
    let testPer1 = ["a";"a";]
    let list1 = permutations(["a";"a";], 1)
    for i in 0..(list1.Length - 1) do
        Assert.Equal(testPer1[i], list1[i])

let ``permutations test 7`` () =
    let testPer1 = ["aa";"aa";"aa";"aa"]
    let list1 = permutations(["a";"a";], 2)
    for i in 0..(list1.Length - 1) do
        Assert.Equal(testPer1[i], list1[i])

let ``permutations test 8`` () =
    let testPer1 = ["a";"a";"c";]
    let list1 = permutations(["a";"a";"c";], 1)
    for i in 0..(list1.Length - 1) do
        Assert.Equal(testPer1[i], list1[i])
        
[<Fact>]
let ``permutations test 9`` () =
    let testPer2 =["aa";"aa";"ac";"aa";"aa";"ac";"ca";"ca";"cc";]
    let list1 = permutations(["a";"a";"c";], 2)
    for i in 0..(list1.Length - 1) do
        Assert.True(testPer2[i] = list1[i])


[<Fact>]
let `` power set test 0`` () =
    let testPow1 = powerSet([])
    Assert.True(testPow1.IsEmpty)
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
    
