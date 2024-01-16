// Q00
let double = ((*) 2)
let doubleList = List.map double
// ---

// Q1, Q2
let print x = printfn "%A" x
print <| doubleList [ 1; 2; 5 ]
// ---

// Q03
let divideSwapped y x = x / y
let divideByTwo = divideSwapped 2.0
// ---

// Q04
let divide x y = x / y
let reciprocal = divide 1.0
// ---

// Q05
// Yes - partial functions are only applied to the first parameter
// ---

// Q06
// 1. attempted applying (int->int) on (float)
// 2. attemped applying (string) on (string->int)
// ---

// Q07
let square x = x * x
let add x y = x + y
// (int -> (int -> int))
// ---

// Q08
let add' x =
    let addx y = x + y
    addx
// beacuse the only the last line is the returned value
// all other values are discarded
// ---

// Q09
// no becuse a different addx is returned every time add is called
// or...x is bound only to the add' function
// ---

// Q10
let add3 x y z = x + y + z
let add3' x =
    let add2 y =
        let add1 z = x + y + z
        add1
    add2
// ---

// Q11
// f (1+2) 3 (g 5 (h 4))
// ---

// Q12
// Valid lists
let list0 = []
let list1 = [ 1; 5; 6; 5; -1; 2 ]
let list2 = [ 1.0; 3.12; 0.01; -10.4 ]
let list3 = [ "I've"; "got"; "a"; "little"; "list" ]
let list4 = [ [ 1; 3 ]; [ 1 ]; [ -1; 3; -4 ]; [] ]
let list5 = [ 1..4 ]

// Invalid lists
// let list6 = [1; 0.1; 30.12; 10; 3;]
// let list7 = ["I've";"got";"them";["on";"the";"list"]]

// because they mix types
// ---

// Q13
let squareList = List.map square
let result = squareList [ 1..4 ]
// [1; 4; 9; 16] list<int>
// ---

// Q14
let incrementList = List.map (fun x -> x + 1)
// ---

// Q15
// gr4test: int -> bool
// elGraterThanFour: list<int> -> list<bool>
// ---

// Q16
let elEqualToTwo = List.map (fun x -> x = 2)
// ---

// Q17
// functions will output list of partial functions
// ---

// Q18
// f: 'a -> 'b
// List.map f: list<'a> -> list<'b>
// List.map: ('a -> 'b) -> (list<'a> -> list<'b>)
// ---

// Q19
let makePair a b = (a, b)
let lst1 = [ 1..5 ]
let lst2 = [ 6..10 ]
let pairWithOne = makePair 1
let combinationsWithOne = List.map pairWithOne lst2
// [ (1, 6); (1, 7); (1, 8); (1, 9); (1, 10) ]
// ---

// Q20
// we want a function that takes a value and spits out combination with ls2
let makeColumn x =
    let makePairX y = makePair x y
    List.map makePairX lst2
print (makeColumn 1)
// ---

// Q21
let makeColumn' x =
    let pairWithX = makePair x
    List.map pairWithX
// equivalent because the first one is just makeColumn' applied to a list
// ---

// Q22
let allPairs lst1 lst2 =
    let makeColumn y =
        let makePairX x = makePair x y
        List.map makePairX lst1 // gives list of partially applied functions

    List.map makeColumn lst2 // gives list of list of fully applied functions = tuples
// makePairX: int -> (int -> (int * int))
// List.map makePairX lst1: list<(int -> (int * int))>
// makeColumn': int -> list<(int -> (int * int))>
// List.map makeColumn' lst2: list<list<int * int>>
// allPairs: list<int> -> list<int> -> list<list<int * int>>

// alternatively...
// let lst1 = [1..5]
// let lst2 = [6..10]
// let allPairs lsta lstb =
//     let makePair a b =
//         (a,b)
//     let makeColumn lst x =
//         let pairWithX = makePair x    // Partially apply makePair.
//         List.map pairWithX lst        // Use this to generate a list of pairs with x
//     List.map (makeColumn lstb) lsta
print (allPairs lst1 lst2)
// ---

// Q23
let allpairs lsta lstb =
    List.map ((fun lst x -> List.map ((fun a b -> (b, a)) x) lst) lsta) lstb
// ---

// Q24
// makePair: 'a -> 'b -> ('a * 'b)
// ---

// Q25
// allThings: ('a -> 'b -> 'c) -> list<'a> -> list<'b> -> list<'c'>
// ---

// Q26
// sum: list<int> -> int
// ---

// Q27
// 10
// ---

// Q28
let mul a b = a * b
let prod = List.reduce mul
let lst = [ 1..4 ]
let q28 = prod lst
print q28
// ---

// Q29
let fact n = if n = 0 then 1 else prod [ 1..n ]
print (fact 0)
// ---

// Q30
let q30 =
    [ [ "Oh"; "thoughtless"; "mortals! " ]; [ "ever"; "blind" ]; [ "to"; "fate" ] ]
let addList lst =
    if lst = [] then [] else List.reduce List.append lst
print (addList q30)
// rememver to consider the empty list!
// ---

// Q31
let allPairs' lst1 lst2 =
    let makeColumn y =
        let makePairX x = makePair x y
        List.map makePairX lst1
    addList (List.map makeColumn lst2)
// or just use List.collect
print (allPairs' lst1 lst2)
// ---

// Q32
let q32 = [ 1; 5; 3; 8; 9 ]
let maxOfList lst =
    List.reduce max lst
print (maxOfList q32)
// ---

// Q33
let rep x =
    [ x; x ]
let rep2 lst = 
    List.collect rep lst
let q33 = rep2 [ 1; 2; 3 ]

print q33
// ---

// Q34
let replXNTimes n x =
    List.map (fun _ -> x) [1..n]
let replListNTimes n lst = 
    List.collect (replXNTimes n) lst
    // List.reduce List.append (List.map (replXNTimes n) lst)
let q34 = [ 1; 2; 3 ]
print (replListNTimes 4 q34)
// ---

// Q35
let isPos x = 
    if x < 0
    then [ ]
    else [ x ]
let filterIsPos lst =
    List.collect isPos lst

print (filterIsPos [ -1; -2; 4; 3; -7; 5 ])
// ---

// Q36
let anLst = [ 1.0; 0.5; 0.0; 0.25 ]
let poly alst x =
    alst 
    |> List.mapi (fun n a -> a * (x ** (float n)))
    |> List.reduce (+)

print (poly anLst 1.0)
// ---

// Q37
let term x n an =
    an * (x ** (float n))
// ---

// Q38
let poly' alst x =
    List.reduce (+) (List.mapi (term x) alst)
// let poly x =
//     let coeffs = [1.0;0.5;0.0;0.25]
//     let term n an =
//         an * (x ** (float n))
//     List.mapi term coeffs
//     |> List.reduce (+)
print (poly' anLst 1.0)
// ---

// Q39
let x = [ -2.0; -1.0; 0.0; 1.0; 2.0 ]
let coeff = [ 0.0; 0.0; 1.0 ]
let q39 = List.map (poly coeff) x
// ---

// Q40
// same as Q36 and Q38
// ---
