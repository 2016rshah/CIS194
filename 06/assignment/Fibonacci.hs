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

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))


