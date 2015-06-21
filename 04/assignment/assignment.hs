{-# OPTIONS_GHC -Wall #-}

--Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl (*) 1 . filter even . map (subtract 2) 

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate h 
    where h n 
            | even n = n `div` 2
            | otherwise = 3 * n + 1

--Exercise 3
xor :: [Bool] -> Bool --true if odd number of trues
xor = foldr (\_ acc -> not acc) False . filter (\x -> x == True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\curr acc -> (f curr) : acc) []