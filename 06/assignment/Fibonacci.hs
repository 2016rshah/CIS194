{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

--Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

--Exercise 2 (I'll come back to this)
--fibs2 :: [Integer]
--fibs2 = map (uncurry (+)) (zip (tail fibs2) (tail tail fibs2))

--fibs2 = [0, 1]
--fibs2 = (last fibs2) + (secondLast fibs2) : fibs2 

--Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : (streamToList b) 

instance Show a => Show (Stream a) where
	show = show . take 20 . streamToList 

showX :: Show a => Int -> Stream a -> String
showX x = show . take x . streamToList

--Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons app (streamFromSeed f app) 
	where app = f x   

--Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) (-1)

negs :: Stream Integer --for testing purposes
negs = streamFromSeed (subtract 1) (1)

interleaveStreams :: Stream a -> Stream a -> Stream a --Not sure how to use this?
interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))

highestPower :: Integer -> Integer
highestPower x = if x `elem` ps 
	then ((+) 1 . fromIntegral . length . takeWhile (not . (==) x)) ps
	else 0
	where ps = [2 ^ y | y <-[1..x]]

tailStream :: Stream a -> Stream a
tailStream (Cons a b) = b

headStream :: Stream a -> a --Implemented just 'cause
headStream (Cons x y) = x

ruler :: Stream Integer
ruler = streamMap highestPower (tailStream nats)


--Exercise 6

type SI = Stream Integer

x :: SI
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num SI where
	fromInteger n = Cons n (streamRepeat 0) 
	negate (Cons i s) = Cons (-i) (negate s)
	(+) (Cons a as) (Cons b bs) = Cons (a + b) (as + bs)
	(*) (Cons a as) bbs@(Cons b bs) = Cons (a * b) ((streamMap (* a) bs) + (as * bbs))

instance Fractional SI where
	(/) (Cons a as) (Cons b bs) = q
		where q = Cons (a `div` b) (streamMap (`div` b) (as - q * bs))

--I don't understand the derivation for the divide operator or the derivation for fibs3

fibs3 :: SI
fibs3 = x / (1 - x - x * x)

--Exercise 7
data Matrix = Matrix ((Integer, Integer), (Integer, Integer))

instance Show Matrix where
	show (Matrix ((a, b), (c, d))) = "[" ++ show a ++ ", " ++ show b ++ "]\n[" ++ show c ++ ", " ++ show d ++ "]"

instance Num Matrix where
	(*) (Matrix ((a, b), (c, d))) (Matrix ((e, f), (g, h))) = Matrix (
			((a*e + b*g), (a*f + b*h)), 
			((c*e + d*g), (c*f + d*h))
		)

f :: Matrix
f = Matrix ((1, 1), (1, 0))

l :: Matrix -> Integer
l (Matrix m) = (snd . snd) m

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = l (f^(n))

--0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377