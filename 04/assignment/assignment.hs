{-# OPTIONS_GHC -Wall #-}

--Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl1 (*) 1 . filter even . map (subtract 2) 

--This function takes the sum of the even hailstone numbers from what I understand
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . evenHailstones
    where 
        evenHailstones :: Integer -> [Integer]
        evenHailstones 1 = [0]
        evenHailstones n
            | even n = n : (evenHailstones (n `div` 2))
            | otherwise = evenHailstones (3 * n + 1)  