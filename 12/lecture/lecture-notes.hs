--Monads are used to do action 2 based on the results of action 1

-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b

--Monad class defines return, which is same thing as pure. Just wrap a value in the monad

--Also defines "bind" with >>=. This is the most important part of the monad.
--The first argument is of the type `m a`. It can be referred to as a "monadic value" or a "computation" or a "mobit"
--The second argument is a function that will accept an argument based on the results of the first computation
--Based on this result, it will return a new mobit.
--So all (>>=) really does is put together two mobits to produce a larger one
--which first runs one and then the other, returning the result of the second one.
--The all-important twist is that we get to decide which mobit to run second based on the output from the first.

-- instance Monad Maybe where
--   return  = Just
--   Nothing >>= _ = Nothing
--   Just x  >>= k = k x


check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing


halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing

--Monads help us chain together functions like check and halve
--Typically you can't chain them because the Maybe's get in the way
--You need to extract the maybe and pass it to the next function

--For example, if you try
--ex00 = halve (check 7)
--You run into a type error that can't match Int with Maybe Int
--Instead, do like this:
ex01 = return 7 >>= check >>= halve
ex01' = halve =<< check 7 -- in my opinion, this is a more intuitive way of looking at it
ex02 = return 12 >>= check >>= halve
ex03 = return 12 >>= halve >>= check

--Use the first monadic computation to determine the second monadic computation
--But after using the first monadic computation, discard it and only return the second one

-- instance Monad [] where
--   return x = [x]
--   xs >>= k = concat (map k xs)

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

ex04 = [10,20,30] >>= addOneOrTwo


--Using the monad definition, you can define `sequence`
--Sequence basically runs a series of monads and accumulates the results into a list
--Take a list of, say 10, monadic values and turn it into a single monadic value in the form of a list of 10 regular values

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (ma:mas) =
    ma >>= \a -> --Run the first monadic value
      sequence mas >>= \as -> --sequence the rest of the elements and run the resulting monad
        return (a:as) --combine the two things and wrap it in a monad

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = sequence (replicate n m)
--Replicates a series of monadic values and pulls the monad out to the front of the list

