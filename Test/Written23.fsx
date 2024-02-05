// For each of the unquoted identifiers qN below state its inferred F# type, 
// or that it does not have an F# type because there is a type error,
// If the code results in a value restriction when compiled state this.
// You may assume that given code is compiled (all together) in this module
// There are 20 questions. The questions make up 50% of the overall test mark 
// and have equal weight.

// You need to know: id is the identity function: let id x = x

let q0 a b c = [a,b],c

let q1 = List.map (List.map (fun n -> n+1))

let q2 x y z w = (x y) (z w)

let q4 f = id (fun x -> f x)

let rec q5 n f = if n = 0 then id else f (q5 (n-1) f)

let q6 x y = x <| x y

let q7 = [1 , []]

let q8 = id q7

let q9' = None
// Option.get: gets the value associated with the option
let q9 = 1 + Option.get q9'

let q10' = []
let q10 = id []
let q10'' = q10 @ [1]

let q11 b x y z = if b then x y else x z

//----------------------------------------------------------------------------//
// For each type T below state the number of distinct values of that type |T|.
// For function types with a finite range two functions f, g are distinct if
// there exists x such that f x <> g x
//----------------------------------------------------------------------------//

type Q12 = unit option option

type Q13 = Result<Unit, bool>*Result<bool -> bool -> bool, bool -> Unit>

// suppose |'A|=a, |'B| = b
type Q14<'A,'B> = Result<Result<'A,'B>,Option<Result<'A,'B>>> * bool

//----------------------------------------------------------------------------//
// For each identifier below state its value
//----------------------------------------------------------------------------//

let q15 =
    6
    |> (*) 5
    |> (+) ((*) 3 4) 
    |> (-) 0

let q16 =
    [0..10]
    |> List.partition (fun n -> n % 3 = 0)
    |> fun (a,b) -> List.rev a @ b

let q17 = 
    let thing x = function 
        | x when x = 1 -> 2 
        | _ -> 4
    thing 3 1

let q18 =
    let double f x = f ( f x)
    let f a b c d =
        (a b) (c d)
    let g = f double id double double
    g (fun x -> x + 1) 3

let q19 =
    let rec f a b =
        match a,b with
        | _, 0 -> 1
        | 0, _ -> b * f 0 (b - 1)
        | n, _ -> n + (f (n-1) b)
    f 3 4

//----------------------------------------------------------------------------//
// For each type T below state the number of distinct values of that type |T|.
// For function types with a finite range two functions f, g are distinct if
// there exists x such that f x <> g x
//----------------------------------------------------------------------------//

type Q12' = unit option option 
// 3

type Q13' = Result<Unit, bool>*Result<bool -> bool -> bool, bool -> Unit> 
// 3*(2^4+1)=54

// suppose |'A|=a, |'B| = b
type Q14'<'A,'B> = Result<Result<'A,'B>,Option<Result<'A,'B>>> * bool 
// ((a+b)+(a+b+1))*2=4a+4b+2
