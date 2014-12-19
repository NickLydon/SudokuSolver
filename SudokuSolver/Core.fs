module SudokuSolver

module Map =

    let union map1 combine map2 =
        map1 
        |> Map.fold (fun acc k v ->
            let newVal = 
                match (acc |> Map.tryFind k) with
                | None -> v
                | Some v' -> combine v v'
            acc |> Map.add k newVal
        ) map2

type Grid = Map<(int*int),int option>

let grid =
    [
        [ None;   None;    Some 6;None;   None;   Some 4; None;   Some 8; Some 1 ]
        [ None;   Some 8;  None;  None;   None;   Some 5; Some 2; None;   None   ]
        [ None;   None;    Some 9;Some 7; None;   None;   Some 4; None;   None   ]
        [ Some 8; Some 1;  None;  None;   None;   None;   None;   None;   None   ]
        [ None;   None;    Some 2;None;   Some 7; None;   Some 6; None;   None   ]
        [ None;   None;    None;  None;   None;   None;   None;   Some 7; Some 4 ]
        [ None;   None;    Some 5;None;   None;   Some 2; Some 7; None;   None   ]
        [ None;   None;    Some 1;Some 9; None;   None;   None;   Some 2; None   ]
        [ Some 2; Some 7;  None;  Some 6; None;   None;   Some 3; None;   None   ]
    ]

let getCoordinates grid =
    grid 
    |> List.mapi(fun y list ->
        list 
        |> List.mapi(fun x value ->
            ((x,y), value)
        )
    )
    |> List.concat
    |> Map.ofList

let emptyGrid =
    let rawValues = List.replicate 9 (List.replicate 9 Option<int>.None)
    getCoordinates rawValues

let getPopulated (grid:Grid) =
    grid 
    |> Map.fold(fun (unpop, pop) position value ->
        let cell = (position,value)
        match value with
        | None -> (cell::unpop, pop)
        | Some _ -> (unpop, cell::pop)
    ) ([],[])

let allCoordinates =
    [for y in [0..8] do
        for x in [0..8] do
            yield (x,y)]

let isComplete (grid:Grid) =
    grid 
    |> Map.forall(fun _ v -> Option.isSome v)

let range = Set [1..9]
let onlyPossibleNumber eliminated =
    eliminated 
    |> Set.difference range
    |> Set.toList
    |> List.head

let reduceGrid reducer (grid:Grid) =
    Seq.unfold(fun (curr, prev) ->
        if curr = prev then None
        else 
            let reduced = grid |> reducer
            Some(reduced, (reduced, curr))
    ) (grid, emptyGrid)
    |> Seq.last

let nineGrids =
    [for x in 0..3..8 do
        for y in 0..3..8 do
            yield (x,y)]
    |> List.map(fun (x,y) ->
        [for x' in x..x+2 do
            for y' in y..y+2 do
                yield (x',y')]
    )

let sweep (grid:Grid) =
    let (unpop, _) = getPopulated grid

    let crossEliminatedNumbers =
        unpop
        |> List.map(fun ((x,y),_) ->
            let cross = 
                let eightContiguousCells cellPosition = [0..8] |> List.map cellPosition
                let xs = eightContiguousCells (fun x' -> (x',y))
                let ys = eightContiguousCells (fun y' -> (x,y'))
                xs |> List.append ys
                |> List.filter(fun p -> p <> (x,y))

            cross
            |> List.map(fun p -> ((x,y), grid |> Map.find p))
            |> List.filter (snd >> Option.isSome)
            |> List.map(fun (a,b) -> (a, Option.get b))
        )
        |> List.concat
        |> List.fold(fun acc (position,value) ->
            match acc |> Map.tryFind position with
            | Some(a) -> acc |> Map.add position (a |> Set.add value)
            | None -> acc |> Map.add position (value |> Set.singleton)
        ) Map.empty

    let gridEliminatedNumbers =
        nineGrids
        |> List.map(fun g ->
            let valuesInGrid = g |> List.map (fun k -> (k, grid |> Map.find k))

            let filledNumbers = 
                valuesInGrid 
                |> List.filter (snd >> Option.isSome)
                |> List.map (fun (position,value) -> value |> Option.get)
                |> Set.ofList

            let unfilledPositions = 
                valuesInGrid
                |> List.filter (snd >> Option.isNone)
                |> List.map fst

            unfilledPositions
            |> List.map (fun p -> (p, filledNumbers))
        )
        |> List.concat
        |> Map.ofList

    crossEliminatedNumbers 
    |> Map.union gridEliminatedNumbers Set.union
    |> Map.filter(fun k v -> v.Count = 8)
    |> Map.fold(fun acc k v ->
        acc
        |> Map.add k (Some(v |> onlyPossibleNumber))
    ) grid

let doIt g =
    getCoordinates g
    |> reduceGrid sweep