module Tick2

//---------------------------Tick2 PartA skeleton code-------------------------------//

module PartACase1 =
    type MscBoundariesType =
        { Distinction: int
          Merit: int
          Pass: int
          Fail: int }

    type MengBoundariesType =
        { First: int
          UpperSecond: int
          LowerSecond: int
          Fail: int }

    type BengBoundariesType =
        { First: int
          UpperSecond: int
          LowerSecond: int
          Third: int
          Fail: int }

    let mscBoundaries: MscBoundariesType =
        { Distinction = 70
          Merit = 60
          Pass = 50
          Fail = 0 }

    let mengBoundaries: MengBoundariesType =
        { First = 70
          UpperSecond = 60
          LowerSecond = 50
          Fail = 0 }

    let bengBoundaries: BengBoundariesType =
        { First = 70
          UpperSecond = 60
          LowerSecond = 50
          Third = 40
          Fail = 0 }

module PartACase2 =
    type Boundaries =
        { Bound70: option<string>
          Bound60: option<string>
          Bound50: option<string>
          Bound40: option<string>
          Bound0: option<string> }

    let mscBoundaries: Boundaries =
        { Bound70 = Some "Distinction"
          Bound60 = Some "Merit"
          Bound50 = Some "Pass"
          Bound40 = None
          Bound0 = Some "Fail" }

    let mengBoundaries: Boundaries =
        { Bound70 = Some "First"
          Bound60 = Some "UpperSecond"
          Bound50 = Some "LowerSecond"
          Bound40 = None
          Bound0 = Some "Fail" }

    let bengBoundaries: Boundaries =
        { Bound70 = Some "First"
          Bound60 = Some "UpperSecond"
          Bound50 = Some "LowerSecond"
          Bound40 = Some "Third"
          Bound0 = Some "Fail" }

module PartACase3 =
    let mscBoundaries = [ "Distinction", 70.; "Merit", 60.; "Pass", 50.; "Fail", 0. ]

    let mengBoundaries =
        [ "First", 70.; "UpperSecond", 60.; "LowerSecond", 50.; "Fail", 0. ]

    let bengBoundaries =
        [ "First", 70.; "UpperSecond", 60.; "LowerSecond", 50.; "Third", 40.; "Fail", 0. ]


//---------------------------Tick2 PartB case 2 skeleton code-------------------------------//

module PartBCase2 =

    open PartACase2

    /// Return as a Ok string the name of the correct classification for a student on given course with given mark.
    /// Return Error if course or mark are not possible (marks must be in range 100 - 0).
    let classify (course: string) (mark: float) : Result<string, string> =

        let findBoundaryList (courseName: string) : Result<Boundaries, string> =
            let boundaryMap: Map<string, Boundaries> =
                Map [ "MSc", mscBoundaries; "MEng", mengBoundaries; "BEng", bengBoundaries ]

            match Map.tryFind courseName boundaryMap with
            | None -> Error(sprintf "given course name (%A) undefined " courseName)
            | Some bound -> Ok bound

        let findClassification (mark: float) (boundaryList: Result<Boundaries, string>) : Result<string, string> =
            match boundaryList with
            | Error message -> Error message
            | Ok bound ->
                match mark with
                | v when (70. <= v && v <= 100.) -> Ok(Option.get bound.Bound70)
                | v when (60. <= v && v < 70.) -> Ok(Option.get bound.Bound60)
                | v when (50. <= v && v < 60.) -> Ok(Option.get bound.Bound50)
                | v when (40. <= v && v < 50.) -> Ok(Option.defaultValue "Fail" bound.Bound40)
                | v when (0. <= v && v < 50.) -> Ok(Option.get bound.Bound0)
                | _ -> Error(sprintf "given mark (%A) outside range of 0 - 100" mark)

        course 
        |> findBoundaryList 
        |> findClassification mark


//---------------------------Tick2 PartB case 3 skeleton code-------------------------------//

module PartBCase3 =

    open PartACase3

    /// Return as a Ok string the name of the correct classification for a studen on given course with given mark.
    /// Return Error if course or mark are not possible (marks must be in range 100 - 0).
    let classify (course: string) (mark: float) : Result<string, string> =

        let findBoundaryList (courseName: string) : Result<list<string * float>, string> =
            let boundaryMap: Map<string, list<string * float>> =
                Map [ "MSc", mscBoundaries; "MEng", mengBoundaries; "BEng", bengBoundaries ]

            match Map.tryFind courseName boundaryMap with
            | None -> Error(sprintf "given course name (%A) undefined" course)
            | Some bound -> Ok bound

        let findClassification (mark: float) (boundResult: Result<list<string * float>, string>) : Result<option<string * float>, string> =
            match boundResult with
            | Error message -> Error message
            | Ok bound -> Ok(List.tryFind (fun (_, lb) -> mark > lb) bound)

        let checkClassification (boundResult: Result<option<string * float>, string>) : Result<string, string> =
            match boundResult with
            | Error message -> Error message
            | Ok None -> Error(sprintf "given mark (%A) outside range of 0 - 100" mark)
            | Ok (Some (cl, _)) when (mark < 0. || 100. < mark) -> Error(sprintf "given mark (%A) outside range of 0 - 100" mark)
            | Ok (Some (cl, _)) -> Ok cl

        course 
        |> findBoundaryList 
        |> findClassification mark 
        |> checkClassification


//------------------------------------Tick2 PartC skeleton code-----------------------------------//

module PartC =
    open PartACase3
    open PartBCase3

    type Marks = { Mark1: float } // simplified set of marks (just one mark) used for compilation of code

    /// Return the total mark for a student used to determine classification.
    /// Return None if the course is not valid or any of the marks areoutside the correct range 0 - 100.
    /// marks:  constituent marks of student on given course.
    /// course: name of course student is on
    let markTotal (marks: Marks) (course: string) : float option =
        match course with
        | "MEng"
        | "BEng"
        | "MSc" when marks.Mark1 <= 100.0 && marks.Mark1 >= 0.0 -> Some marks.Mark1 // in this case with only one mark, student total is just the mark!
        | _ -> None

    /// Operation:
    /// 1. Return an error if boundary is not a valid boundary for course.
    /// 2. Return IsAboveBoundary = true if total is above or equal to boundary
    /// 3. Return Uplift = Some uplift if total is in the valid possible uplift range (0 - -2.5%) of boundary.
    let upliftFunc
        (marks: Marks)
        (boundary: string)
        (course: string)
        : Result<{| IsAboveBoundary: bool
                    Uplift: float option |}, string>
        =
        // Use markTotal to calculate total from marks
        // Also return an error if markTotal fails to calculate a mark
        // Ok return type is an anonymous record see link in WS2.
        // upliftFunc is assumed (when implemented) to take boundary info from a value defined above
        // with whatever data structure is used for it. In Part C you do not implement
        // upliftFunc and so need not consider any of this.
        failwithf "Not Implemented" // do not change - implementation not required

    /// Given a list of boundaries, and a course, and a student's marks:
    /// Return the student classification, or an error message if there is any error in the data.
    /// boundaries: name only, subfunctions will know boundary marks based on course, this function needs only the results of calling its subfunctions.
    let classifyAndUplift (boundaries: string list) (course: string) (marks: Marks) : Result<string, string> =
        // Assume that the student can be within possible uplift range of at most one boundary.
        // Assume that classify is correct unless student is within uplift range of a given boundary,
        // If student is within uplift range of a boundary `boundaryName` work out classification as:
        // let total = markTotal marks course
        // let effectiveMark = total + upliftFunc boundaryName course
        // let className = classify course effectiveMark
        let findMarkTotal (course: string) (marks: Marks) : Result<float, string> =
            match markTotal marks course with
            | Some markTotalValue -> Ok markTotalValue
            | None -> Error "course is not valid or any of the marks are outside the correct range of 0 - 100"

        let findEffectiveMark (boundaries: string list) (course: string) (marks: Marks) (total: Result<float, string>) : Result<float, string> =
            let uplift : Result<float, string> =
                let upliftFolder (upliftState: Result<float, string>) (boundary: string) : Result<float, string> =
                    match upliftState with
                    | Error message -> Error message
                    | Ok upliftStateVal -> 
                        match upliftFunc marks boundary course with
                        | Error message -> Error message
                        | Ok info -> if info.IsAboveBoundary then (Ok upliftStateVal) else (Ok (Option.defaultValue 0. info.Uplift))
                
                List.fold upliftFolder (Ok 0.) boundaries

            match total with
            | Error message -> Error message
            | Ok totalValue ->
                match uplift with
                | Error message -> Error message
                | Ok upliftValue -> Ok (totalValue + upliftValue)

        let findClassification (course: string) (effectiveMark: Result<float, string>) : Result<string, string> =
            match effectiveMark with
            | Error message -> Error message
            | Ok effectiveMarkValue -> classify course effectiveMarkValue

        (course, marks) 
        ||> findMarkTotal 
        |> findEffectiveMark boundaries course marks
        |> findClassification course


//------------------------------Simple test data and functions---------------------------------//

module TestClassify =
    /// test data comaptible with the Tick 2 problem
    let classifyUnitTests =
        [ "MEng", 75.0, Ok "First"
          "MSc", 75.0, Ok "Distinction"
          "BEng", 75.0, Ok "First"
          "MEng", 65.0, Ok "UpperSecond"
          "MSc", 65.0, Ok "Merit"
          "BEng", 65.0, Ok "UpperSecond"
          "MEng", 55.0, Ok "LowerSecond"
          "MSc", 55.0, Ok "Pass"
          "BEng", 55.0, Ok "LowerSecond"
          "MEng", 45.0, Ok "Fail"
          "MSc", 45.0, Ok "Fail"
          "BEng", 45.0, Ok "Third"
          "BEng", 35.0, Ok "Fail" ]

    let runClassifyTests unitTests classify testName =
        unitTests
        |> List.map (fun (data as (course, mark, _)) -> classify course mark, data)
        |> List.filter (fun (actualClass, (_, _, className)) -> actualClass <> className)
        |> function
            | [] -> printfn $"all '{testName}' tests passed."
            | fails ->
                fails
                |> List.iter (fun (actual, (course, mark, className)) ->
                    printfn
                        $"Test Failed: {course}, {mark}, expected className={className}, \
                                          actual className={actual}")


//-------------------------------------------------------------------------------------------//
//---------------------------------Run Part B tests------------------------------------------//
//-------------------------------------------------------------------------------------------//

open TestClassify

let runTests () =
    runClassifyTests classifyUnitTests PartBCase2.classify "Case2"
    runClassifyTests classifyUnitTests PartBCase3.classify "Case3"


//-------------------------------------------------------------------------------------------//
//---------------------------------Tick2 Part X Skeleton code--------------------------------//
//-------------------------------------------------------------------------------------------//

module PartX =
    type Lens<'A, 'B> = ('A -> 'B) * ('B -> 'A -> 'A)

    let lensMap (lens: Lens<'A, 'B>) (f: 'B -> 'B) (a: 'A) = (fst lens a |> f |> snd lens) a

    let mapCAndB (lensC: Lens<'A, 'C>) (lensB: Lens<'A, 'B>) (fc: 'C -> 'C) (fb: 'B -> 'B) =
        lensMap lensC fc >> lensMap lensB fb

    let combineLens (l1: Lens<'A, 'B>) (l2: Lens<'B, 'C>) : Lens<'A, 'C> =
        let get (a: 'A) = fst l2 (fst l1 a)
        let set (c: 'C) (a: 'A) = snd l1 (snd l2 c (fst l1 a)) a
        get, set

