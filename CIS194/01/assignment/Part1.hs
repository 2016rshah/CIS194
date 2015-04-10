--Validating Credit Card Numbers
{-# OPTIONS_GHC -Wall #-}

--Exercise 1
{-
Example: toDigits 1234 == [1,2,3,4]
Example: toDigitsRev 1234 == [4,3,2,1]
Example: toDigits 0 == []
Example: toDigits (-17) == []
-}
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
	| n <= 0 	= []
	| otherwise	= n `mod` 10 : toDigitsRev(n `div` 10)


--Excercise 2
{-
Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
Example: doubleEveryOther [1,2,3] == [1,4,3]
-}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (zipWith (*) (cycle [1,2]) (reverse xs))
--Reverse it first because you double every other from the right
--Then zipWith with multiply it with the array that is circular between 1 and 2
--The cycle array is infinite but zipWith only goes to the smallest so it is bounded by the length of the arg
--Then reverse it again to give the answer back the original way


--Exercise 3
{-
Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
-}
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (splitDigits xs)

splitDigits :: [Integer] -> [Integer]
splitDigits [] = []
splitDigits (x:xs) = (toDigits x) ++ splitDigits xs