-- Machine-sized integers
i :: Int
i = -78

-- Arbitrary-precision integers
n :: Integer
n = 1234567890987654321987340982334987349872349874534

--Compiler does not yell because it is lazy. Will yell when you try to use it
y :: Int
y = y+1

--badArith1 = i + n --bad because no implicit type conversion between Int and Integer
betterArith1 = i + (fromIntegral n) -- you can do fromIntegral on either variable. 

--Difference between double precision floating point numbers and single precision floating point numbers?

--badArith2 = i / i --bad because `/` is only used for floating point division. Instead use `div`
betterArith2 = i `div` i

--If expressions are of the form: if p then t else f
--If expressions are not used much though. 

--Example function. Evaluate from top down for match
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

--Example of guards. 
hailstone :: Integer -> Integer
hailstone n  -- no equal sign here
    | n `mod` 2 == 0 = n `div` 2
    | otherwise = 3*n + 1 --otherwise is just syntactic sugar for True. 
    --Evaluated from top down so otherwise catches everything not already caught by other guards

--More complex example of guards.
foo :: Integer -> Integer
foo 0 = 16
foo 1
    | "Haskell" > "C"   = 3
    | otherwise         = 4
foo n
    | n < 0             = 0
    | n `mod` 17 == 2   = -43
    | otherwise         = n + 3

--Given example of isEven
--isEven :: Integer -> Bool
--isEven n 
--  | n `mod` 2 == 0 = True
--  | otherwise      = False
--Given is bad. Fix it?
isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

p :: (Int, Char)
p = (3, 'z')

subPair :: (Int, Int) -> Int
subPair (x, y) = x-y --Pattern matching x and y makes sure they are subtracted correctly

--Multiple arguments:
f :: Int -> Int -> Int -> Int
f x y z = x + y + z
--Note that functions take precedence over infix operations. 
--Thus f 3 n+1 7 really means (f 3 n) + (1 7) which is wrong. Instead write f 3 (n+1) 7

--Lists are pretty fancy, but you've already covered that in LYAH
--Also note that since strings are just lists of chars you can apply all list operations to strings

--The cons operator (:) adds something to the beginning and returns list. 
a = 1:[] -- [1]
b = 1 : (2 : []) -- [1,2]
c = 1 : (2 : (3 : [])) -- [1,2,3]
d = 0 : c -- [0,1,2,3]

--Function with lists and the cons operator
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

--Pattern matching with cons
intListLength :: [Integer] -> Integer
intListLength []    = 0
--intListLength (x:xs)= 1 + intListLength xs -- If it matches the pattern of having one element appended to the rest of the list, the length is 1 + the length of the list without the element
intListLength (_:xs)= 1 + intListLength xs -- Since we never use the variable x we can replace it with _

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []          = []
sumEveryTwo (x:[])      = [x]
--sumEveryTwo (x:(y:zs))  = (x + y) : sumEveryTwo zs
--sumEveryTwo (x:y:zs)  = (x + y) : sumEveryTwo zs --Get rid of the parens in argument
sumEveryTwo (x:y:zs)  = x + y : sumEveryTwo zs --Get rid of parens in function

--Break functions up into smaller parts!
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1

main = print $ "Week One of CIS194"



