// Q01
// when set of values are passed into functions as a tuple
// (not really clear what this question is asking)
// ---

// Q02
// (int -> int -> int) -> string -> string
// (takes two ints after curried, hopefully)
// ---

// Q03
// call method to cast input List types into Array type, or just change the
// Array function to List functions, but some fixes of difference of methods is
// required; Maps are not directly translatable
// ---

// Q04
// map is the map function, Map is the Map module
// ---

// Q05
// usually curring (or removing the last param of a function) will genrally
// work; but for a wildcard type like "%A", it cannot be used as part of another
// type without being explicit at compile time
// ---

// Q06
// parameter is the vvariable local to the fucntion
// argument is the value passed into the parameter
// ---

// Q07
type A1 =
    | Monday1 of Unit
    | Tuesday1 of Unit

type A2 =
    | Monday2
    | Tuesday2
// practically A1 and A2 are the same thing; but in fact Monday1 and Tuesday1
// are constructors for type Unit, whereas Monday2 and Tuesday2 are just
// constants; A2 is preferred
// ---

// Q08
// MyOrchardDU: {(NumAppleTrees) * uint32, (NumPearTrees) * uint32}
// A: {Stick, Hat}
// B: {Thomas, Peter, John, Emily}
// C: {Clarke, Smith, Thatcher}
// MyDU: {Stick, Hat, (Thomas, Clarke), (Thomas, Smith), ..., Weird}
// or better..if "u" is the disjoint union oeprator
// MyOrchardDU: { Z u Z }
// MyDU: { A u BxC u W }, where W = {Weird}
// ---

// Q09
// Use a third type, like None, it is problematic because NumAppleTrees 0u and
// NumPearTrees 0u are different to the disjoint union but same for all other
// purposes
// ---

// Q10
// 2 + 4 * 3 + 1 = 15
// ---

// Q11
// Thing Stick
// Thing Hat
// Person(Thomas, Clarke)
// Person(Thomas, Smith)
// Person(Thomas, Thatcher)
// Person(Peter, Clarke)
// Person(Peter, Smith)
// Person(Peter, Thatcher)
// Person(John, Clarke)
// Person(John, Smith)
// Person(John, Thatcher)
// Person(Emily, Clarke)
// Person(Emily, Smith)
// Person(Emily, Thatcher)
// Weird
// ---

// Q12
type Course =
    | MEng
    | BEng
    | MSc

type Boundary =
    | Fail
    | Pass
    | Third
    | UpperSecond
    | LowerSecond
    | First
    | Merit
    | Distinction
// ---

// Q13
// all three, all three needs to check if the course passed in is valid; if a
// type was passed it can be easily typechecked
// ---

// Q14
type EeidType = Eeid of int
type NameType = Name of string

type RoleType =
    | EEE
    | EIE
    | Staff

type YearType =
    | Year of int
    | Undefined

type CidType = Cid of int
// or more practically: consider whether further wrapping is essential
type TRole =
    | EEE
    | EIE
    | Staff

type TUGYear =
    | One
    | Two
    | Three
    | Four

type Person =
    { eeid: int
      name: string
      role: TRole
      year: TUGYear option
      cid: int }
// ---

// Q15
// TUGYear -> Option<TUGYear>
// ---

// Q16
// bool would be much preferable, same size as domain
// ---

// Q17
type Player' =
    | PlayerOne
    | PlayerTwo

type 'a OrWin' =
    | Win of Player'
    | Game of 'a
// ---

// Q18
type PlayerScore' =
    | Score0
    | Score15
    | Score30
    | Score40
    | Deuce
    | Advantage

type TennisScore' =
    { playerOne: PlayerScore'
      playerTwo: PlayerScore' }

// in a much better way...
type Player =
    | PlayerOne
    | PlayerTwo

type PlayerPoints =
    | Zero
    | Fifteen
    | Thirty
    | Forty

type TennisGameScore =
    | Points of PlayerPoints * PlayerPoints
    | Advantage of Player
    | Deuce

type 'a OrWin =
    | Win of Player
    | Game of 'a

type MaybeWonTennisGameScore = TennisGameScore OrWin
// ---

// Q19
let scorePoint (player: Player) (score: TennisGameScore) : MaybeWonTennisGameScore =
    let increasePoint (points: PlayerPoints) : PlayerPoints =
        match points with
        | Zero -> Fifteen
        | Fifteen -> Thirty
        | Thirty -> Forty
        | _ -> failwithf "attempt to increase past 40"

    match score with
    | Points(sval1, sval2) ->
        match (sval1, sval2) with
        | Forty, snd ->
            if player = PlayerOne then Win player
            else if snd = Thirty then Game Deuce
            else Game(Points(Forty, (increasePoint snd)))
        | fst, Forty ->
            if player = PlayerTwo then Win player
            else if fst = Thirty then Game Deuce
            else Game(Points((increasePoint fst), Forty))
        | fst, snd ->
            if player = PlayerOne then
                Game(Points((increasePoint fst), snd))
            else
                Game(Points(fst, (increasePoint snd)))
    | Advantage splayer -> if splayer = player then Win splayer else Game Deuce
    | Deuce -> Game(Advantage player)
// or you can attempt to normalize each time...
// provided answer does not typecheck
// ---

// Q20
// (the given link to the code was broken, but...)
// basically they all are enum types, making them discriminated unions makes
// them less prone to errors
// ---

// Q21
// 2 component records of ComponentType
// 2 connection records of Connection
// 4 port records of Port
// ---

// Q22
// use an object for each type and then union them together
// alternatively...
// use a struct which has all fields and then set to NULL when not used
// look into...
// std::variant
// ---

// From here on, see TestingProject
// (somehow I can't get it to work in vscode, more digging later)

// Q23
// should be: (a && b) = not ((not a) || (not b))
// due to precedence of operators
// ---

// Q24
// 6 sigmal => covered 99.7%
// 1 - e^(-6) = 0.9975
// K = 6
// ---

// Q25
// >> is the forward composition function
// i.e. f >> g >> x = g(f(x))
// since composition is associative, this will always be true
// ---

// Q26
// Tests run seperately and cannot be combined. It is not obvious to see what
// ran and what failed.
// ---

// Q27
// Only failed test cases are printed out. Test statistics are also given.
// ---

// Q28
// Runs everything that is tagged with [<Test>]
// ---

// Q29
let getEndsOf (lst: List<string>) : string =
    match lst.Length with
    | n when n < 2 -> ""
    | _ -> lst[0] + " " + lst[lst.Length - 1]

getEndsOf [ "1"; "2"; "3" ]
// ---

// Q30
let x = [ 1; 2; 3 ]
printfn "%s" (x.ToString())
// ---
