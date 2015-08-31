--Pattern matching drives evaluation
--Things won't be evaluated until they need to be within the pattern matching
f1 :: Maybe a -> [Maybe a]
f1 m = [m,m] --m does NOT need to be evaluated, it is just passed along

f2 :: Maybe a -> [Maybe a]
f2 Nothing = []
f2 (Just x) = [x, x] --it needs to be evaluated to check if its a nothing
--But x still is not evaluated, just passed along

--Short circuiting is built into Haskell

--Effectively infinite data structures, like the potential moves in a chess game can be efficient in Haskell because
--You don't need to store the entire tree, just the parts you are going to explore

--The knapsack problem https://en.wikipedia.org/wiki/Knapsack_problem
