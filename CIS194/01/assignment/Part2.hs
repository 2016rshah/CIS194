--Towers of Hanoi
{-# OPTIONS_GHC -Wall #-}

--Exercise 5
{-
Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
-}
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

lenHanoi :: Integer -> Peg -> Peg -> Peg -> Int
lenHanoi n a b c = length (hanoi n a b c)



hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 2 a b c _ = [(a, c), (a, b), (c, b)]
hanoi4 3 a b c d = [(a, d)] ++ (hanoi4 2 a b c d) ++ [(d, b)]
hanoi4 n a b c d = hanoi4 (n - 1) a c b d ++ [(a, b)] ++ hanoi4 (n-1) c b a d

lenHanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> Int
lenHanoi4 n a b c d = length (hanoi4 n a b c d)