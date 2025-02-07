module Program

// module Program = let [<EntryPoint>] main _ = 0

// For more information see https://aka.ms/fsharp-console-apps
// printfn "Hello from F#"

// Read 5.1 and 5.2 (will should be able to finish covering 5.2 next time)
//
// * Code up a routine that generates n-arry string of a given length L. In other words, given an alphabet of chars (strings of length 1) generate a list (set) of all strings of a given length that can be constructed. Naturally it might be a good idea to use recursion. If your alphabet consists of 2 things you will be generating binary strings effectively.
//
// * For the above exercise you will want to write a power function to compute n^k. What is a good way to implement it? Say we have n^10. We could just write a loop to multiply n by itself 10 times, right? Is this efficient?
// How about if we wrote it as:
//
// d = n*n // n^2
// dd = d * d // n^4
// result = dd * dd * d // n^10
//
// The above requires 4 multiplications instead of 9. Seems faster, doesn’t it?
//
// Can you see a pattern? Can you write an efficient generic power function using this approach? Use the function to test the previous exercise.
//
// * Using F#’s Sets code up: permutations, combinations. This is an already pretty hard, but if you can do this improve your permutations algorithm using mutable structures as discussed in class. Basically given a set of chars of size N and length K generate all nPk possibilities. Do the same of nCk.
//
// * Now generate a PowerSet of a given set
//
// * Given that you know the formula for the number of elements in the output of each problem you can test at least the size of the generated output is correct. To do that please write some good routines for computing nPk (e.g. no need to compute the full factorials when you can compute the falling powers of the form 5*4*3 and stop; no need to divide). Basically think of an efficient implementation for these things.
//
// * All the code should be tested, especially the edge cases.
//



    let powerFunction number power =
        let numberSquared number = number * number
        
        let mutable newNumber = number
        let mutable remainder = power 
        let mutable closestFactorOfTwo = 1
        let mutable closestPowerOfTwo = 1
        let mutable counter = 0
        
        while remainder > closestFactorOfTwo do
            remainder <- remainder - closestFactorOfTwo
            closestFactorOfTwo <- closestFactorOfTwo * 2
            closestPowerOfTwo <- closestPowerOfTwo + 1
            newNumber <- numberSquared newNumber
            counter <- counter + 1
        
        
        for i in 1..((remainder - 1)/2) do
            newNumber <- newNumber * (numberSquared number)
            counter <- counter + 1
        
        if (remainder - 1) % 2 = 1 then
            newNumber <- newNumber * number
            counter <- counter + 1
        
        newNumber
        // counter

    (*
        iterations = floor(log base 2 (k)) + remainder / 2
        where remainder = k - (closest power of 2 < k)
        so x = (closest power of 2 < k) must be k/2 < x < k
        so iterations < log base 2 (k) + k / 4 + 1
        the + 1 is to account for if iterations is not an integer
    *)


    let permutations (alph:list<string>, n) =
        match alph.Length, n with
        |0, _ -> failwith "no empty lists allowed"
        |_, 0 -> []
        |1, _ -> [alph.Head]
        |_, 1 -> alph
        |_, _ -> 
            let mutable n = n - 1
            let mutable output = []
            for i in alph do
                for j in alph do
                    output <- output@[i + j]
            
            let mutable output2 = output
            
            while n > 1 do
                output2 <- output
                output <- []
                for i in alph do
                    for j in output2 do
                        output <- output@[i + j]
                output2 <- output
                n <- n - 1
            output
        
        
    let test11 = ["a";"b";"c";]
    let test12 =["aa";"ab";"ac";"ba";"bb";"bc";"ca";"cb";"cc";]
    let mutable test13 =[]
    for i in test12 do
        for j in test11 do
            test13 <- test13@[i + j]

    let list0 = ["a";"b";"c";]
    let list1 = (permutations(list0, 1))
    let list2 = (permutations(list0, 2))
    let list3 = (permutations(list0, 3))



    // For more information see https://aka.ms/fsharp-console-apps
    let powerSet(set:list<string>) =
        let rec powerSet (set:list<string>, output:list<string>, count) =
            let mutable nextPart = [set.Head]
            for i in output do
                nextPart <- nextPart @ [i + set.Head]
            if set.Length > 1 then
                powerSet(set.Tail, output@nextPart, count + 1)
            else
                output@nextPart
        
        match set with
        |[h] -> [h]
        |h::m::t when not(t = []) -> powerSet(t, [h; h + m; m], 0)
        |h::[t] -> [h; h + t; t]
        
        
    
    
    



