namespace SudokuSolver

type Grid = Map<(int*int),int option>

module Solver =

    let printGrid len (grid:Grid) =
        let sorted =
            grid
            |> Map.toSeq
            |> Seq.sortBy (fst >> snd)
            |> Seq.map (snd >> function | Some x -> string x |> char | None -> ' ')

        [for i in 0..len-1 do
            yield sorted |> Seq.skip (i*len) |> Seq.take len |> Seq.toList]
        |> List.iter (printfn "%A\r\n")


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

    let emptyGrid len =
        let rawValues = List.replicate len (List.replicate len Option<int>.None)
        getCoordinates rawValues

    let getPopulated (grid:Grid) =
        grid 
        |> Map.fold(fun (unpop, pop) position value ->
            let cell = (position,value)
            match value with
            | None -> (cell::unpop, pop)
            | Some _ -> (unpop, cell::pop)
        ) ([],[])

    let isComplete (grid:Grid) =
        grid 
        |> Map.forall(fun _ v -> Option.isSome v)

    let onlyPossibleNumber range eliminated =
        eliminated 
        |> Set.difference range
        |> Set.toList
        |> List.head

    let reduceGrid reducer len (grid:Grid) =
        Seq.unfold(fun (curr, prev) ->
            if curr = prev then None
            else 
                let reduced = curr |> reducer
                Some(reduced, (reduced, curr))
        ) (grid, emptyGrid len)
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
            |> Map.add k (Some(v |> onlyPossibleNumber(Set([1..9]))))
        ) grid

    let doIt g =
        getCoordinates g
        |> reduceGrid sweep 9