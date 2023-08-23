module AulaT where
    data Section = Section{ getA ::Int
                            ,getB ::Int
                            ,getC ::Int }

    type RoadSystem = [Section]

    heathrowtoLondon :: RoadSystem
    heathrowtoLondon = [Section 50 10 30
                        ,Section 5 90 20
                        ,Section 40 2 25
                        ,Section 10 8 0]

    data Label = A | B | C deriving (Show)

    type Path = [(Label,Int)]

    roadStep :: (Path,Path) -> Section -> (Path,Path)
    roadStep (pathA,pathB) (Section a b c)= 
        
        let timeA = sum (map snd pathA)
            timeB = sum (map snd pathB)
            forwardTimeToA = timeA + a
            forwardTimeToB = timeB + b
            crosstimeToA = forwardTimeToB + c
            crosstimeToB = forwardTimeToA + c
            newPathToA = if forwardTimeToA <= crosstimeToA then (A, a) : pathA else (C,c) : (B,b) : pathB
            newPathToB = if forwardTimeToB <= crosstimeToB then (B, b) : pathB else (C,c) : (A,a) : pathA
            in(newPathToA, newPathToB)

    optimalpath :: RoadSystem -> Path
    optimalpath roadSystem = 
        let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
        in if sum (map snd bestAPath) <= sum (map snd bestBPath)
            then reverse bestAPath
            else reverse bestBPath 


