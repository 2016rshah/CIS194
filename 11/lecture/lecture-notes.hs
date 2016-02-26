{-# LANGUAGE AllowAmbiguousTypes #-}

import Control.Applicative

(*>)       :: Applicative f => f a -> f b -> f b
x *> y = (\xx yy -> yy) <$> x <*> y

f1 :: Int -> Maybe String
f1 x = Just "yes"

--[1,2,3] -> Just ["yes", "yes", "yes"]

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = (\xs -> sequenceA (map f xs)) 
--Below is solution before solving sequenceA
--mapA f = foldr (\x y -> (:) <$> f x <*> y) (pure []) 

sequenceA  :: Applicative f => [f a] -> f [a]
sequenceA = foldr (\x y -> (:) <$> x <*> y) (pure [])

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n v = sequenceA (replicate n v)