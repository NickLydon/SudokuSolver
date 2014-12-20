namespace Extensions

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