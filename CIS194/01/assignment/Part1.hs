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
--doubleEveryOther :: [Integer] -> [Integer]
--doubleEveryOther xs