namespace SudokuSolver.Tests

open NUnit.Framework
open SudokuSolver
open HtmlAgilityPack

type CoreTests() = 

    let parseTestGrid g =
        g
        |> List.map (List.map
            (function 
            | ' ' -> None 
            | a -> Some(int (string a)))
        )

    let loadTestGrid fileName = 
        let htmlDoc = new HtmlDocument()
        htmlDoc.LoadHtml(
            System.IO.File.ReadAllText(fileName))
        htmlDoc.DocumentNode.FirstChild.ChildNodes
        |> Seq.map(fun n ->
            n.ChildNodes
            |> Seq.map(fun n -> 
                match n.FirstChild.Attributes.["value"] with
                | null -> None
                | v -> Some(System.Int32.Parse v.Value)
            )
            |> Seq.toList
        )
        |> Seq.toList

    [<Test>]
    member this.``should give co-ordinates for empty grid with No(ne) values``() =
        let actual = Solver.emptyGrid 3 
        let expected = [(0,0);(0,1);(0,2);(1,0);(1,1);(1,2);(2,0);(2,1);(2,2)] |> List.map(fun p -> (p, Option<int>.None)) |> Map.ofList
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``should give coordinates given a grid's values``() =
        let testGrid = [
            [Some 1; None]
            [None; Some 2]
            ]
        let actual = Solver.getCoordinates testGrid
        let expected = [((0,0), Some 1);((0,1), None);((1,0),None);((1,1),Some 2)] |> Map.ofList
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``gets unpopulated cells``() =
        let testGrid = [((0,1), Some 1); ((1,0), None)] |> Map.ofList
        let (unpop, pop) = Solver.getPopulated testGrid
        Assert.That(unpop, Is.EqualTo([((1,0),Option<int>.None)]))
        Assert.That(pop, Is.EqualTo([((0,1), Some 1)]))

    [<Test>]
    member this.``indicates completed when all cells have a value``() =
        let testGrid = [((0,1), Some 1); ((1,1), Some 2)] |> Map.ofList
        let actual = Solver.isComplete testGrid
        Assert.That(actual, Is.True)

    [<Test>]
    member this.``indicates incomplete when cells are missing value``() =
        let testGrid = [((0,1), None); ((1,1), Some 2)] |> Map.ofList
        let actual = Solver.isComplete testGrid
        Assert.That(actual, Is.False)

    [<Test>]
    member this.``should split grid into 9``() =
        Solver.nineGrids
        |> List.zip [
            [(0, 0); (0, 1); (0, 2); (1, 0); (1, 1); (1, 2); (2, 0); (2, 1); (2, 2)]
            [(0, 3); (0, 4); (0, 5); (1, 3); (1, 4); (1, 5); (2, 3); (2, 4); (2, 5)]
            [(0, 6); (0, 7); (0, 8); (1, 6); (1, 7); (1, 8); (2, 6); (2, 7); (2, 8)]
            [(3, 0); (3, 1); (3, 2); (4, 0); (4, 1); (4, 2); (5, 0); (5, 1); (5, 2)]
            [(3, 3); (3, 4); (3, 5); (4, 3); (4, 4); (4, 5); (5, 3); (5, 4); (5, 5)]
            [(3, 6); (3, 7); (3, 8); (4, 6); (4, 7); (4, 8); (5, 6); (5, 7); (5, 8)]
            [(6, 0); (6, 1); (6, 2); (7, 0); (7, 1); (7, 2); (8, 0); (8, 1); (8, 2)]
            [(6, 3); (6, 4); (6, 5); (7, 3); (7, 4); (7, 5); (8, 3); (8, 4); (8, 5)]
            [(6, 6); (6, 7); (6, 8); (7, 6); (7, 7); (7, 8); (8, 6); (8, 7); (8, 8)]
        ]
        |> List.iter(fun (actual, expected) -> CollectionAssert.AreEquivalent(expected, actual))

    [<Test>]
    member this.``should reduce grid until no more values can be filled``() =
        let i = ref 0
        let testGrid = [((1,0), Some 0)] |> Map.ofList
        let actual = Solver.reduceGrid (fun g -> 
            incr i
            g
            |> Map.map(fun k value ->
                match !i with
                | 3 -> value
                | _ -> 
                    value |> Option.bind(fun v -> Some(v+1)))) 1 testGrid

        Assert.That(!i, Is.EqualTo(3))
        Assert.That(actual, Is.EqualTo([((1,0), Some 2)] |> Map.ofList))

    [<Test>]
    member x.``should solve easy puzzles``() =
        let actual = 
            loadTestGrid "Easy.html"
            |> Solver.doIt

        let expected =
            [
                ['5'; '9'; '8'; '3'; '4'; '7'; '2'; '1'; '6']
                ['6'; '1'; '4'; '2'; '5'; '8'; '9'; '7'; '3']
                ['3'; '2'; '7'; '1'; '6'; '9'; '8'; '5'; '4']
                ['2'; '4'; '5'; '9'; '8'; '1'; '6'; '3'; '7']
                ['7'; '8'; '6'; '4'; '3'; '5'; '1'; '2'; '9']
                ['1'; '3'; '9'; '7'; '2'; '6'; '5'; '4'; '8']
                ['9'; '7'; '2'; '8'; '1'; '3'; '4'; '6'; '5']
                ['8'; '5'; '1'; '6'; '7'; '4'; '3'; '9'; '2']
                ['4'; '6'; '3'; '5'; '9'; '2'; '7'; '8'; '1']
            ]
            |> parseTestGrid
            |> Solver.getCoordinates

        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member x.``should solve medium puzzles``() =
        let actual = 
            loadTestGrid "Medium.html"
            |> Solver.doIt

        let expected =
            [
                ['9'; '7'; '3'; '6'; '5'; '8'; '2'; '4'; '1']
                ['5'; '2'; '1'; '7'; '9'; '4'; '8'; '6'; '3']
                ['8'; '4'; '6'; '2'; '3'; '1'; '7'; '5'; '9']
                ['3'; '5'; '8'; '4'; '6'; '7'; '1'; '9'; '2']
                ['2'; '9'; '4'; '5'; '1'; '3'; '6'; '7'; '8']
                ['1'; '6'; '7'; '9'; '8'; '2'; '4'; '3'; '5']
                ['6'; '8'; '9'; '1'; '7'; '5'; '3'; '2'; '4']
                ['4'; '1'; '5'; '3'; '2'; '6'; '9'; '8'; '7']
                ['7'; '3'; '2'; '8'; '4'; '9'; '5'; '1'; '6']
            ]
            |> parseTestGrid
            |> Solver.getCoordinates

        Assert.That(actual, Is.EqualTo(expected))