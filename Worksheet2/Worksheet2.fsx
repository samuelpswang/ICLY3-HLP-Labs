// Q01
let tuple1 = 1, 9
open System.Collections
let tuple2 = "Bob Smith", 27
let tuple3 = "Bob", "Smith", 27.32
let tuple4 = 36, 6, 142
let tuple5 = "Westminster SW", 1
let tuple6 = 36, true
// tuple 2 and tuple 5
// ---

// Q02
type Person =
    { Age: int
      FirstName: string
      FamilyName: string
      Email: string }

let tom =
    { Age = 61
      FirstName = "Thomas"
      FamilyName = "Clarke"
      Email = "tom@xx.co.uk" }

let tomsTwin =
    { tom with
        FirstName = "Bob"
        Email = "bob@yy.co.uk" }

let toTuple p =
    p.Age, p.FirstName, p.FamilyName, p.Email

let ageUp p = { p with Age = p.Age + 1 }

let printFamily =
    function
    | { FamilyName = "Clarke" } as p -> printfn "%s is in Tom's family" p.FirstName
    | { FamilyName = "Smith" } as p -> printfn "%s is in the Smith family" p.FirstName
    | { FirstName = name; FamilyName = nameF } -> printfn "This person is %s %s" name nameF

let newEmail (email: string) (contact: Person) = { contact with Email = email }
// ---

// Q03
let tup1 = 2, 4
let tup2 = -1, 4
let tup3 = 2, 4

tup1 = tup2
tup1 <> tup2
tup1 < tup3

// tuples are compared in lexicography order from left to right
// ---

// Q04
type InnerRecordType = { X: int; Y: int }

type RecordType =
    { Q: InnerRecordType
      R: InnerRecordType }

// lens is a tuple which
// 1) first: gets data, so get 'B type from 'A
// 2) second: writes data, so take 'B, write to 'A, but is immutable so it spits out a new 'A
type Lens<'A, 'B> = ('A -> 'B) * ('B -> 'A -> 'A)

// applying the previous gets us this
type MyLens = ((RecordType -> InnerRecordType) * (InnerRecordType -> RecordType -> RecordType))
// ---

// Q05
let lensMap (lens: Lens<'A, 'B>) (f: 'B -> 'B) (a: 'A) : 'A =
    let getter, setter = lens
    setter (f (getter a)) a
// ---

// Q06
let mapCAndB (lensC: Lens<'A, 'C>) (lensB: Lens<'A, 'B>) (fc: 'C -> 'C) (fb: 'B -> 'B) (a: 'A) : 'A =
    let getC, setC = lensC
    let getB, setB = lensB
    let a' = setC (fc (getC a)) a
    setB (fb (getB a')) a'
// alternatively and in a much better way -
let mapCAndB' (lensC: Lens<'A, 'C>) (lensB: Lens<'A, 'B>) (fc: 'C -> 'C) (fb: 'B -> 'B) (a: 'A) : 'A =
    a |> lensMap lensC fc |> lensMap lensB fb
// ---

// Q07
// done within worksheet
// ---

// Q08
// because the (>) is a function that takes in two operators (>) x y
// where true is returned when x > y; since we are currying the (>) function,
// we pass in 0 for x such that if 0 > y the function will return true
// ---

// Q09
let lst1 = [ 1; -1; 6; 0; -3 ]
let lst2 = [ 2; 5; 65; 3 ]
let firstNegative = List.tryFind ((>) 0)
let firstNegative' = List.find ((>) 0)
// tryFind returns a optional integer: Option<int>
// find returns an integer: int and throws an exception when it fails
// ---

// Q10
// using lists is not as obvious or readable to the user
// ---

// Q11
let customFind (l: list<int>) : int =
    let neg = List.tryFind ((>) 0) l
    let pos = List.tryFind ((<) 0) l
    Option.defaultValue 0 (Option.orElse pos neg)

// test
customFind [ 1; -2; 3; 4 ]
customFind [ 1; 2; 3; 4 ]
customFind [ 0; 0 ]
customFind []

// improvement via thunk
// let foo lst =
//     List.tryFind ((>) 0) lst
//     |> Option.orElseWith (fun () -> List.tryFind ((<) 0) lst)
//     |> Option.defaultValue 0
// ---

// Q12
let rec filter (f: 'A -> bool) (l: list<'A>) : list<'A> =
    match l with
    | head :: tail ->
        if (f head) then
            head :: (filter f tail)
        else
            (filter f tail)
    | [] -> []

// alternatively
let rec filter' f lst =
    match lst with
    | hd :: tl when f hd -> hd :: (filter f tl)
    | _ :: tl -> filter f tl
    | [] -> []

// test
filter ((>) 0) [ 1; -2; 3; -4 ]
filter' ((>) 0) [ 1; -2; 3; -4 ]
// ---

// Q13
let fact (n: int) = [ 1..n ] |> List.fold (*) 1
// test
fact 0
fact 5
// ---

// Q14
let reverse (lin: list<'A>) (lout: list<'A>) : list<'A> =
    List.fold (fun lout el -> List.append [ el ] lout) lout lin
// or more elegantly
let reverse' (lin: list<'A>) : list<'A> =
    List.fold (fun ls el -> el :: ls) [] lin
// test
reverse [ 1; 2; 3 ] []
reverse' [ 1; 2; 3 ]
// ---

// Q15
// rule 1 is the subset
// subset of {A, ...B} = {{A, B0}, {A, B1}, ..., {A, BN}, subset of {B}}
// ---

// Q16
// we are assumingly starting with the subset of an empty set
// if the subset of an empty set is nothing, then every following set will be
// nothing, the answer then will just be []
// --

// Q17
let rec subsetFull (set: 'A list) : 'A list list =
    match set with
    | head :: tail ->
        List.allPairs [ [ head ]; [] ] (subsetFull tail)
        |> List.map (fun (x, y) -> List.append x y)
    | [] -> [ [] ]
// test
subsetFull [ 1; 2; 3 ]
// ---

// Q18
// base case is when [] is hit
// ---

// Q19
// there is a deeper call stack with recursion, unlike in fold function is
// resolved when it is pushed; or - fold is bottom-up, and recursion is top-down
// ---

// Q20
let subsetTail (set: 'A list) : 'A list list =
    let appendEach (item: 'A) (list: list<list<'A>>) : list<list<'A>> =
        let augList = List.map (List.append [ item ]) list
        List.append list augList

    let rec subsetUtil (set: 'A list) (curr: 'A list list) : 'A list list =
        match set with
        | head :: tail -> subsetUtil tail (appendEach head curr)
        | [] -> curr

    subsetUtil set [ [] ]
// or more compactly
let subsetTail' lst =
    let rec subsets' l subs =
        match l with
        | hd :: tl -> List.allPairs subs [ [ hd ]; [] ] |> List.map ((<||) List.append) |> subsets' tl
        | [] -> subs

    subsets' lst [ [] ]
// test
subsetTail [ 1; 2; 3 ]
// --

// Q21
let len (list: 'A list) : int =
    (0, list) ||> List.fold (fun state item -> state + 1)
// test
len [ 1; 2; 3; 4 ]
// ---

// Q22
let subLists (list: 'A list) : 'A list list =
    ([ [] ], list)
    ||> List.fold (fun state item -> List.allPairs [ []; [ item ] ] state |> List.map (fun (x, y) -> List.append x y))
// test
subLists [ 1; 2; 3 ]
// ---

// Q23
let randomSeq (seed: int) (n: int) : int list =
    (seed, [ 1..n ]) ||> List.scan (fun seed _ -> seed * 1103515245 + 12345)
// test
randomSeq 1 10
// ---

// Q24
// all are similar; depend on readability
// ---

// Q25
let s: Result<obj, string> =
    Error <| sprintf "x in hex is 0x%x, x in decimal is %d" 23 2
// ---

// Q26
// no, first pipeline operator is associated to the left, so it
// ---

// Q27
// it works in any pipelines because it takes any type
// ---

// Q28
let printPipe x =
    printfn "%A" x
    x

let testPipe x =
    x
    |> List.pairwise
    |> printPipe // A
    |> List.indexed
    |> List.filter (fun (i, _) -> i % 2 <> 0)
    |> printPipe // B
    |> List.map (fun (a, (b, c)) -> a + b + c)
    |> List.sum
// whole of A will be printed before whole of B
// ---

// Q29
// roughly the same, personally I think the second sample is clearer
// ---

// Q30
// max, min, sum -> must not be empty
// zip -> lists mush be same length
// take, skip -> length must be greater than input number
// ---

// Q31
let a: Map<string, int> = Map.empty
let a' = Map.add "first" 10 a
let a'' = Map.add "second" 20 a'

let b =
    Map.ofList [ "April", 30; "June", 30; "September", 30; "November", 30; "February", 28 ]

// try
a''["first"]
a''["second"]
// a''[""] -> key given was not present in Map
// a''[10] -> error due to type mismatch

// try
let a''' = Map.add "" 0 a''
a'''[""]
// ---

// Q32
let months =
    Map.ofList [ "April", 30; "June", 30; "September", 30; "November", 30; "February", 28 ]

let daysOfMonth (month: string) : int =
    months |> Map.tryFind month |> Option.defaultValue 32
// test
daysOfMonth "January"
daysOfMonth "April"
// ---

// Q33
let inverseMap (map: Map<'K, 'V>) : Map<'V, 'K> =
    let addInverseEntry (new_map: Map<'V, 'K>) (old_key: 'K) (old_val: 'V) : Map<'V, 'K> =
        Map.add old_val old_key new_map

    Map.fold addInverseEntry (Map.empty) map
// or better -
let inverseMap' m =
    m |> Map.toList |> List.map (fun (k, v) -> (v, k)) |> Map.ofList
// try
inverseMap (Map.ofList [ 1, "2"; 3, "4" ])
inverseMap' (Map.ofList [ 1, "2"; 3, "4" ])
// ---

// Q34
// Map.find x m
// ---

// Q35
let xm = Map [ "first", "the"; "second", "cat" ]
let ym = Map [ "cat", 7; "the", 3 ]
let zm = Map [ [ 1; 2; 3 ], 5 ]
zm[[ 1; 2; 3 ]]
let ym' = Map.add "fox" 5 ym
// try
ym[xm["first"]]
// ---

// Q36
// xm: Map<string,string>
// ym: Map<string,int>
// zm: Map<list<int>,int>
// ---

// Q37
let getKeys (map: Map<'K, 'V>) : list<'K> = map |> Map.keys |> Seq.toList
let getKeys' (map: Map<'K, 'V>) : list<'K> = map |> Map.toList |> List.map fst
// try
getKeys months
getKeys' months
// ---

// Q38
let data = "The quick brown fox jumps over the lazy dog"

let histogram (data: string) : Map<char, int> =
    let increment (state: Map<char, int>) (item: char) : Map<char, int> =
        match Map.tryFind item state with
        | None -> Map.add item 1 state
        | Some count -> Map.add item (count + 1) state

    data |> Seq.toList |> List.fold increment Map.empty
// alternatively...
let histogram' (data: string) =
    data
    |> Seq.toList
    |> List.groupBy id
    |> Map.ofList
    |> Map.map (fun k v -> List.length v)
// try
histogram data
|> Map.iter (fun c n -> printfn "Number of '%c' characters = %d" c n)
// ---

// Q39
let gap (list: list<'int>) : int =
    list
    |> List.pairwise
    |> List.map (fun (item1, item2) -> item2 - item1)
    |> List.append [ list[0] - list[(List.length list) - 1] ]
    |> List.max
// test
gap [ 10; 1; 3; 4 ]
// ---

// Q40
let gap' (list: list<'int>) : int =
    (List.pairwise
     >> List.map (fun (item1, item2) -> item2 - item1)
     >> List.append [ list[0] - list[(List.length list) - 1] ]
     >> List.max)
        list
// test
gap' [ 10; 1; 3; 4 ]
// ---

// Q41
let modal (list: list<'T>) : (list<'T> * int) =
    if List.isEmpty list then
        [], 0
    else
        let max_count =
            list
            |> List.countBy (fun x -> x)
            |> List.maxBy (fun (t, c) -> c)
            |> (fun (fst, snd) -> snd)

        list
        |> List.countBy (fun x -> x)
        |> List.map (fun (t, c) -> if c = max_count then [ t ] else [])
        |> List.reduce List.append
        |> (fun els -> (els, max_count))
// try
modal ([]: list<string>)
modal [ 1; 2; 2; 3; 4; 4; 4 ]
// ---

// Q42
let sequences (a: int) (b: int): list<list<int>> = 
    [a..b]
    |> subsetTail 
    |> List.map (List.sort)
    |> List.filter (fun list -> List.length list <> 0)
// or more simply
let sequences' (a: int) (b: int) =
    List.allPairs [a..b] [a..b]
    |> List.filter (fun (a,b) -> a <= b)
    |> List.map (fun (a,b) ->[a..b])
// try
sequences 3 5
sequences' 3 5
// ---

// Q43
let insertElement (list: list<'T>) (toInsert: 'T) (at: int): list<'T> =
    if List.length list >= at then
        list
        |> List.splitAt at
        |> fun (fst, snd) -> List.reduce List.append [fst; [toInsert]; snd]
    else
        failwithf "wrong index"
let insertList (list: list<'T>) (toInsert: list<'T>) (at: int): list<'T> =
    if List.length list > at then
        list
        |> List.splitAt at
        |> fun (fst, snd) -> List.reduce List.append [fst; toInsert; snd]
    else
        failwithf "wrong index"
// try
insertElement [1; 2; 3] 0 0
insertList [3; 4; 5] [1; 2;] 0
// ---

// Q44
let insertElement' (list: list<'T>) (toInsert: 'T) (at: int): list<'T> =
    if List.length list > at then list[0..at-1] @ [toInsert] @ list[at..]
    else failwithf "wrong index"
// try
insertElement' [0; 1; 3] 2 2 
// ---

// Q45
let merge (list1: list<'T>) (list2: list<'T>): list<'T> =
    let insertElementByOrder (list: list<'T>) (elem: 'T): list<'T> =
        match List.tryFindIndex (fun comp -> elem <= comp) list with
        | None -> 
            if elem < list[0] then insertElement list elem 0
            else insertElement list elem ((List.length list))
        | Some ind -> 
            insertElement list elem ind
    List.fold insertElementByOrder list2 list1
// merge [1; 2; 4; 7] [3; 5; 6]
// merge [0] [-1]
let rec sort (list: list<'T>): list<'T> = 
    if List.length list <= 1 
    then 
        list
    else
        let mid = (int ((List.length list)/2))
        let left, right = List.splitAt mid list
        merge (sort left) (sort right)
sort [1; -3; 2; 4; 5; 0; 3; -2; -1]
// ---


// Q46
let findLongestRow (lol: list<list<'a>>): int =
    lol
    |> List.map List.length
    |> List.max
let padRowReq (n: int) (row: list<int>): list<int> =
    List.map (fun c -> if c-1 < List.length row then row[c-1] else 0) [1..n]
let findMaxOfTwoList (state: list<int>) (row: list<int>): list<int> =
    (state, row)
    ||> List.zip
    |> List.map (fun (a, b) -> if a > b then a else b)
let findLongestCol (lol: list<list<string>>): list<int> =
    let max_col = findLongestRow lol
    lol
    |> List.map (fun row -> List.map String.length row)
    |> List.map (fun row -> padRowReq max_col row)
    |> List.fold findMaxOfTwoList (List.map (fun _ -> 0) [1..max_col])
let padElement (input: string) (n: int): string =
    let charArr = Seq.toList input
    [1..n]
    |> List.map (fun c -> if c-1 < List.length charArr then charArr[c-1] else ' ')
    |> List.toArray
    |> System.String
let padRow (row: list<string>) (req: int): list<string> =
    List.map (fun i -> if i-1 < List.length row then row[i-1] else "") [1..req]
let padElementRow (row: list<string>) (req: list<int>): list<string> =
    List.map (fun i -> padElement row[i-1] req[i-1]) [1..List.length row]
let tabulate (input: list<list<string>>): list<list<string>> =
    let max_col = findLongestRow input
    let col_req = findLongestCol input
    input
    |> List.map (fun row -> padRow row max_col)
    |> List.map (fun row -> padElementRow row col_req)
tabulate [
    ["aa" ; "abc"]
    ["abcde"]
    ["abc"; "a"; "axxx"]
]
// ---

// Q47
let getSliderData (sliderValue: float) =
    let min,max,step =
        match sliderValue with
        |v when (v>=0.000000001 && v<0.00000001) -> 0.000000001,0.0000000099,0.0000000001
        |v when (v>=0.00000001 && v<0.0000001) -> 0.00000001,0.000000099,0.000000001
        |v when (v>=0.0000001 && v<0.000001) -> 0.0000001,0.00000099,0.00000001
        |v when (v>=0.000001 && v<0.00001) -> 0.000001,0.0000099,0.0000001
        |v when (v>=0.00001 && v<0.0001) -> 0.00001,0.000099,0.000001
        |v when (v>=0.0001 && v<0.001) -> 0.0001,0.00099,0.00001
        |v when (v>=0.001 && v<0.01) -> 0.001,0.0099,0.0001
        |v when (v>=0.01 && v<0.1) -> 0.01,0.009,0.001
        |v when (v>=0.1 && v<1) -> 0.1,0.99,0.01
        |v when (v>=1 && v<10) -> 1,9.9,0.1
        |v when (v>=10 && v<100) -> 10,99,1
        |v when (v>=100 && v<1000) -> 100,990,10
        |v when (v>=1000 && v<10000) -> 1000,9900,100
        |v when (v>=10000 && v<100000) -> 10000,99999,1000
        |v when (v>=100000 && v<1000000) -> 100000,999000,10000
        |_ -> 1,10,0.01
    {| MinVal = min ; MaxVal = max ; Step = step |}
let makeDecadeData (low: float) (high: float): list<float*float> =
    [low..high]
    |> List.map (fun exp -> 10.0**exp)
    |> List.pairwise
let getSliderData' (sliderValue: float) = 
    (-9., 7.)
    ||> makeDecadeData
    |> List.tryFindIndex (fun (lower, higher) -> lower <= sliderValue && sliderValue < higher)
    |> Option.defaultValue 0
    |> (fun exp -> 
        {| MinVal = 10.**(float exp-9.) ; MaxVal = 10.**(float exp+1.-9.)-10.**(float exp-1.-9.) ; Step = 10.**(float exp-1.-9.) |})
// ---
