greaterThan100 :: [Integer] -> [Integer]
greaterThan100 = filter (\x -> x > 100)
--Anonymous functions
--(\x -> _____)

greaterThan100' :: [Integer] -> [Integer]
greaterThan100' = filter (>100)
-- operator section 
-- given an operator ? like >, <, ==, <=, >=, etc. and a variable y
-- (?y) = (\x -> x?y) like (/10) 5 == 5 / 10 == 0.5
-- (y?) = (\x -> y?x) like (100>) 129 == 100 > 129 == False

--The dot (.) is a function composition operator
--Thus f(g(x)) is written as 
--(f . g) x
floorSum :: (RealFrac a) => [a] -> Integer
floorSum = (floor . sum) --this is currying for an array

--curry applies a function to a tuple formed by the second and third argument
--curry f x y = f (x, y)
--uncurry applies a function with two arguments from a tuple
--uncurry f (x, y) = f x y

--Order function arguments from least to most variation between function calls
--That means you can curry with the last argument and not even pass it in the function definition
--Point free tries to get rid of the pointers in function calls

--Folds accumulate a list into a value
--Check out http://learnyouahaskell.com/higher-order-functions and ctrl-f for folds
--Examples:
--foldr (+) 5 [1,2,3,4] -> 15
--foldr max 18 [3,6,12,4,55,11] -> 55
--foldr max 111 [3,6,12,4,55,11] -> 111