﻿namespace SudokuSolver.Tests

open NUnit.Framework
open SudokuSolver

type MapTests() =
    let idCombiner a b = a

    [<Test>]
    member x.``union should add key value pairs``() =
        let map1 = Map([('a',1)])
        let map2 = Map([('b',2)])
        let expected = map1 |> Map.union map2 idCombiner
        Assert.That(expected, Is.EqualTo(Map([('a',1);('b',2)])))


    [<Test>]
    member x.``union should use combiner function when key found in both``() =
        let map1 = Map([('a',[1])])
        let map2 = Map([('a',[2])])
        let expected = map1 |> Map.union map2 List.append
        Assert.That(expected, Is.EqualTo(Map([('a',[2;1])])))
