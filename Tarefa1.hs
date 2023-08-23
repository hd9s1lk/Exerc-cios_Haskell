module Tarefa1 where

    paresord :: [(Int,Int)] -> [(Int,Int)]
    paresord [] = []
    paresord ((x,y):xs)
            | x < y = (x,y): paresord xs
            | otherwise = paresord xs


    myconcat :: [String] -> String
    myconcat [] = []
    myconcat (x:xs) =  x ++ (myconcat xs)

    myconcat2 :: [String]-> String
    myconcat2 l = foldr (++) "" l

    myconcat3 :: [String] -> String
    myconcat3 l = foldr (\elem cont -> elem ++ cont) "" l

    verdade :: [Bool] -> Bool
    verdade l = foldr (&&) True l

    falso :: [Bool] -> Bool
    falso l = foldr (||) False l

    maximo :: [(Float,Float)] -> [Float]
    maximo [] = []
    maximo ((a,b):xs)
            | a >= b = a: maximo xs
            | otherwise = b:maximo xs

    paresord2 :: [(Double,Double)] -> [(Double,Double)]
    paresord2 l = filter valida l

    valida :: (Double,Double) -> Bool
    valida (x,y) = if x<y then True else False

    maximo2 l = map (\(a,b) -> max a b)

    indicativo :: String -> [String] -> [String]
    indicativo ind telefs = filter (concorda ind) telefs
                        where concorda :: String -> String -> Bool
                              concorda [] _ = True
                              concorda (x:xs) (y:ys) = (x==y) && (concorda xs ys)
                              concorda (x:xs) [] = False

    indicativo2 :: String -> [String] -> [String]
    indicativo2 _ [] = []
    incadicativo2 ind (x:xs)
                    | concorda ind x = x: indicativo2 ind xs
                    | otherwise = indicativo ind xs
                where
                    concorda []_ = True
                    concorda (x:xs) (y:ys) = (x==y) && (concorda xs ys)
                    concorda (x:xs) [] = False

  

    