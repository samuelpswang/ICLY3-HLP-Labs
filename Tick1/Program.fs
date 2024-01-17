open System

//------------------------write your answer function(s) here---------------------//

let fac n =
    if n = 0.0
    then 1.0
    else List.reduce (*) [1..n]

let arg theta =
    if (float theta) % (2.0 * System.Math.PI) > 0
    then (float theta) % (2.0 * System.Math.PI)
    else (float theta) % (2.0 * System.Math.PI) + 2.0 * System.Math.PI

let sin x n =
    [0..n]
    |> List.map (fun i -> if i % 2 = 1 then 1.0 else 0.0)
    |> List.mapi (fun i coeff -> if i % 4 = 3 then -coeff else coeff)
    |> List.mapi (fun i coeff -> (coeff) * (arg x ** i) / (fac i))
    |> List.reduce (+)

let cos x n =
    [0..n]
    |> List.map (fun i -> if i % 2 = 0 then 1.0 else 0.0)
    |> List.mapi (fun i coeff -> if i % 4 = 2 then -coeff else coeff)
    |> List.mapi (fun i coeff -> (coeff) * (arg x ** i) / (fac i))
    |> List.reduce (+)

let polarToCartesianApprox (r,theta) n = 
    (r * cos theta n, r * sin theta n)


//--------------------testbench code - DO NOT CHANGE-----------------------------//

/// used to make generate testbench data
let testInputs =
    let testPolarCoords = List.allPairs [1.;2.] [1.;2.]
    List.allPairs testPolarCoords [0;1;2;3;10]

/// data showing correct results generated with model answer and given here
let testBenchData =
    [
        ((1.0, 1.0), 0, (1.0, 0.0))       
        ((1.0, 2.0), 0, (1.0, 0.0))        
        ((2.0, 1.0), 0, (2.0, 0.0))        
        ((2.0, 2.0), 0, (2.0, 0.0))        
        ((1.0, 1.0), 1, (1.0, 1.0))        
        ((1.0, 2.0), 1, (1.0, 2.0))        
        ((2.0, 1.0), 1, (2.0, 2.0))        
        ((2.0, 2.0), 1, (2.0, 4.0))        
        ((1.0, 1.0), 2, (0.5, 1.0))        
        ((1.0, 2.0), 2, (-1.0, 2.0))        
        ((2.0, 1.0), 2, (1.0, 2.0))        
        ((2.0, 2.0), 2, (-2.0, 4.0))        
        ((1.0, 1.0), 3, (0.5, 0.8333333333))        
        ((1.0, 2.0), 3, (-1.0, 0.6666666667))        
        ((2.0, 1.0), 3, (1.0, 1.666666667))        
        ((2.0, 2.0), 3, (-2.0, 1.333333333))        
        ((1.0, 1.0), 10, (0.5403023038, 0.8414710097))        
        ((1.0, 2.0), 10, (-0.4161552028, 0.9093474427))        
        ((2.0, 1.0), 10, (1.080604608, 1.682942019))        
        ((2.0, 2.0), 10, (-0.8323104056, 1.818694885))
    ]
/// test testFun with testData to see whether actual results are the same as
/// expected results taken from testData
let testBench testData testFun =
    let closeTo f1 f2 = abs (f1 - f2) < 0.000001
    let testItem fn (coords, n, (expectedX,expectedY) as expected) =
        let actualX,actualY as actual = testFun coords n
        if not (closeTo actualX expectedX) || not (closeTo actualY expectedY) then
            printfn "Error: coords=%A, n=%d, expected result=%A, actual result=%A"coords n expected actual
            1
        else
            0
    printfn "Starting tests..."
    let numErrors = List.sumBy (testItem testFun) testData
    printfn "%d tests Passed %d tests failed." (testData.Length - numErrors) numErrors

[<EntryPoint>]
let main argv =
    testBench testBenchData polarToCartesianApprox
    0 // return an integer exit code
