    plant n
        = replicate n newPlant
        where newPlant = 0

    growGarden
        = concatMap growPlant
        where growPlant fruits
                  = succ fruits
                  : plant (fruits + 1)

    solve noPeople noPlants
        = succ . length
        . takeWhile ((<= noPeople) . sum)
        . iterate growGarden
        . plant $ noPlants

    main = interact (unlines . map (show . solveRead . words) . lines)
        where solveRead [x, y] = solve (read x) (read y)