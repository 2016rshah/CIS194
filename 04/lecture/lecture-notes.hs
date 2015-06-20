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
floorSum = (floor . sum) 