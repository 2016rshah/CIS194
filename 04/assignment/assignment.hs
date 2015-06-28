{-# OPTIONS_GHC -Wall #-}
import Data.List

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

--Exercise 2
data Tree a = Leaf 
        | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr i Leaf  
    where
        i :: a -> Tree a -> Tree a
        i x Leaf = Node 0 Leaf x Leaf
        i curr (Node _ Leaf r Leaf) = Node 1 (i curr Leaf) r Leaf 
        i curr (Node _ lt@(Node lh _ _ _) r Leaf) = Node lh lt r (i curr Leaf)
        i curr (Node _ Leaf r rt@(Node rh _ _ _)) = Node rh (i curr Leaf) r rt 
        i curr (Node h lt@(Node lh _ _ _) r rt@(Node rh _ _ _))
            | lh > rh = Node (1+lh) lt r (i curr rt)
            | rh > lh = Node (1+rh) (i curr lt) r rt
            | otherwise=Node (1+h) (i curr lt) r rt 

{--
    Node 3 
        (Node 2 
            (Node 1 
                (Node 0 Leaf 'D' Leaf) 
                'G' 
                Leaf
            ) 
            'I' 
            (Node 1 
                (Node 0 Leaf 'C' Leaf) 
                'E' 
                Leaf
            )
        ) 
        'J' 
        (Node 1 
            (Node 1 
                (Node 0 Leaf 'A' Leaf) 
                'F' 
                Leaf
            ) 
            'H' 
            (Node 0 Leaf 'B' Leaf)
        )
--}

--Exercise 3
xor :: [Bool] -> Bool --true if odd number of trues
xor = foldr (\_ acc -> not acc) False . filter (\x -> x == True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\curr acc -> (f curr) : acc) []

--Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (finalArith . (\\) [1..n] . map iA . ijs) n
    where
        {--cartProd [1,2] [’a’,’b’] == [(1,’a’),(1,’b’),(2,’a’),(2,’b’)]--}
        cartProd :: [a] -> [b] -> [(a, b)]
        cartProd xs ys = [(x,y) | x <- xs, y <- ys]

        finalArith :: [Integer] -> [Integer]
        finalArith = (map (+ 1) . map (* 2))

        ijs :: Integer -> [(Integer, Integer)]
        ijs m = takeWhile (\x -> iA x < m) $ cartProd [1..] [1..]

        iA :: (Integer, Integer) -> Integer
        iA (i, j) = i + j + 2 * i * j
