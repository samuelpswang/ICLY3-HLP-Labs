These questions relate to `DrawBlock`/`SheetSnap` module of `hlp24-project` branch of Issie (latest commit)

1. To reduce replication it is proposed to replace the code in Lines 201-210 by a helper function `makeSnapData` returning type `SnapInfo`. EITHER state the number of `makeSnapData` parameters and its type, or state why the proposal will not work without adding if-then-else or match to the code.

2. To reduce replication it is proposed to replace the code in Lines 72-79 by a helper function `makeUpperOrLower` returning type float. EITHER state the number of `makeUpperOrLower` parameters and its type, OR state why the proposal will not work without adding if-then-else or match to the code.

3. Considering only the code in Lines 36-42, which of the following is true of `Model.SnapSymbols.SnapX.SnapOpt` and `Model.SnapSegments.SnapX.SnapOpt`
    - A. They cannot both be `Some _`
    - B. They can be both `Some _`. This will cause an exception
    - C. They can both be `Some _`. This will not cause an exception

4. Line 149-156. `otherSimilarSymbolData` is:
    - A. A curried function
    - B. A higher order function
    - C. Both
    - D. Neither

5. In Lines 181-194. The output of the `collect` function:
    - A. Can be an empty array
    - B. Can’t be an empty array
    - C. Is an array of arrays - the elements of which might be empty

6. Considering the code in Lines 195-198: Which of the `YSnaps` and `XSnaps` array elements are made from array elements with `ori = BusWireT.Horizontal`?
    - A. `Xsnaps` and `Ysnaps`
    - B. None
    - C. `YSnaps` but not `Xsnaps`
    - D. `XSnaps` but not `Ysnaps`

7. As the result of Line 241:
    - A. Calls are made many times to `List.toArray` and then many times to `getNonZeroAbsSegments`
    - B. Calls are made many times to `getNonZeroAbsSegments` and then many times to `List.toArray`
    - C. Alternating Calls are made to `getNonZeroAbsSegments` and then `List.toArray` - both many times
    - D. Alternating Calls are made to `List.toArray`  and then `getNonZeroAbsSegments` - both many times
    - E. Calls are made to `getNonZeroAbsSegments` many times and then `List.toArray` once

8. Consider function snap1D. Lines 270 - 296. If `autoScrolling = true` and `snapI.SnapOpt = None`, what is the returned value in terms of `snapI` and `pos`?

9. During a symbol drag operation, in either X or Y direction, the screen symbol can be snapped to a fixed position, not moving, while the mouse moves. This is called a “snap”. During a snap, let: `f` = the fixed position coordinate, `m` = the symbol coordinate that would be expected from the mouse position if there was no snap. What is `SnapInfo.SnapOpt` during the snap?
    - A. `None`
    - B. `Some f`
    - C. `Some m`
    - D. `Some (m - f)`
    - E. `Some (f - m)`
